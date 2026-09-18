#!/bin/bash
# ETL diário do Observatório de Impulsionamento (eleições 2026).
#
# Pipeline: baixa o zip do TSE -> load_data_2026.R -> update_database.R --replace
# -> load_totais.R -> update_totais.R -> WAL checkpoint.
#
# Pensado para rodar no container RStudio do CapRover, com o banco num volume
# compartilhado com o app Shiny (shinyverse). Sem passo de deploy: o app lê o
# mesmo arquivo .db que este script escreve.
#
# Config por variáveis de ambiente (todas com default):
#   OBS_DB_PATH   caminho do obseleitoral.db (default: ./obseleitoral.db no dir do repo;
#                 no CapRover: /data/observatorio/obseleitoral.db, ou deixe o default
#                 se o repo inteiro estiver no volume compartilhado)
#   ETL_LOG_DIR   diretório de logs (default: <repo>/logs)
#
# Agendar via cron no container (ver update_directions.md):
#   30 9 * * * /caminho/do/repo/daily_etl.sh
# O próprio script faz `git pull --ff-only` antes do ETL. Isso mantém o cron
# persistido na imagem independente de futuros ajustes do classificador.
#
# O download usa curl_cffi (python) impersonando o TLS do Chrome porque a Akamai
# do cdn.tse.jus.br bloqueia o fingerprint do curl/httr. Requer python3 com
# curl_cffi instalado (pip3 install curl_cffi), ou uv disponível no PATH.

set -euo pipefail

# Ambientes de cron vêm sem locale; em locale C o R não converte os acentos
# do CSV latin1 do TSE e o read.csv aborta com "invalid input"
export LC_ALL="${LC_ALL:-en_US.UTF-8}"

APP_DIR="$(cd "$(dirname "$0")" && pwd)"
LOG_DIR="${ETL_LOG_DIR:-$APP_DIR/logs}"
CODE_STATE="$LOG_DIR/etl_last_successful_code"
RSCRIPT="$(command -v Rscript)"
SQLITE="$(command -v sqlite3 || true)"
ANO=2026
URL="https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_${ANO}.zip"
DB="${OBS_DB_PATH:-$APP_DIR/obseleitoral.db}"
export OBS_DB_PATH="$DB"

mkdir -p "$LOG_DIR"
LOG="$LOG_DIR/etl_$(date +%Y%m%d_%H%M%S).log"
exec >> "$LOG" 2>&1

echo "=== ETL $(date '+%Y-%m-%d %H:%M:%S') | db: $DB ==="
cd "$APP_DIR"

# O cron do container só chama este arquivo; mantenha o classificador e os
# loaders em sincronia com a main sem depender de reconstruir a imagem.
echo "--- Atualização do código"
git -C "$APP_DIR" pull --ff-only
CODE_REV="$(git -C "$APP_DIR" rev-parse HEAD)"
LAST_CODE_REV="$(cat "$CODE_STATE" 2>/dev/null || true)"
echo "código atual: $CODE_REV | último ETL concluído: ${LAST_CODE_REV:-nenhum}"

TMPDIR_ETL=$(mktemp -d)
trap 'rm -rf "$TMPDIR_ETL"' EXIT
ZIP="$TMPDIR_ETL/tse${ANO}.zip"

# python com curl_cffi: direto se instalado, senão via uv
if python3 -c "import curl_cffi" 2>/dev/null; then
  PYRUN=(python3)
elif command -v uv >/dev/null; then
  PYRUN=("$(command -v uv)" run --with curl_cffi python3)
else
  echo "ERRO: preciso de python3 com curl_cffi (pip3 install curl_cffi) ou uv no PATH."
  exit 1
fi

echo "--- Download do TSE"
"${PYRUN[@]}" - "$URL" "$ZIP" <<'PYEOF'
import sys
from curl_cffi import requests
url, dest = sys.argv[1], sys.argv[2]
r = requests.get(url, impersonate="chrome", stream=True, timeout=900)
r.raise_for_status()
n = 0
with open(dest, "wb") as f:
    for chunk in r.iter_content(1024 * 1024):
        f.write(chunk)
        n += len(chunk)
print(f"baixados {n/1e6:.1f} MB")
if n < 1_000_000:
    sys.exit("arquivo suspeito de truncado (<1MB), abortando")
PYEOF

echo "--- load_data_${ANO}.R"
TSE_ZIP_PATH="$ZIP" "$RSCRIPT" "load_data_${ANO}.R"

# Assinatura (linhas + total R$) do snapshot novo vs. o que está no banco:
# se nada mudou, para por aqui; se o snapshot encolheu >20%, aborta (arquivo
# quebrado no TSE é mais provável que uma retificação desse tamanho).
NEW_SIG=$("$RSCRIPT" -e '
  d <- readRDS(sprintf("e%d.rds", '"$ANO"'))
  v <- as.numeric(gsub(",", ".", gsub(".", "", d$VR_DESPESA_CONTRATADA, fixed = TRUE), fixed = TRUE))
  cat(sprintf("%d|%.2f", nrow(d), sum(v, na.rm = TRUE)))')
CUR_SIG=$("$RSCRIPT" -e '
  library(RSQLite)
  con <- dbConnect(SQLite(), Sys.getenv("OBS_DB_PATH"))
  r <- dbGetQuery(con, "SELECT COUNT(*) n, COALESCE(SUM(valor_numeric),0) v FROM despesas WHERE ANO_ELEICAO='"$ANO"'")
  cat(sprintf("%d|%.2f", r$n, r$v)); dbDisconnect(con)')
NEW_ROWS=${NEW_SIG%%|*}
CUR_ROWS=${CUR_SIG%%|*}
echo "snapshot novo: $NEW_SIG | banco atual: $CUR_SIG"

if [[ "$NEW_SIG" == "$CUR_SIG" && "$CODE_REV" == "$LAST_CODE_REV" ]]; then
  echo "Sem mudanças - nada a fazer."
  exit 0
fi
if [[ "$NEW_SIG" == "$CUR_SIG" ]]; then
  echo "Snapshot igual, mas o código do ETL mudou; reprocessando classificação."
fi
if (( NEW_ROWS < CUR_ROWS * 8 / 10 )); then
  echo "ERRO: snapshot com ${NEW_ROWS} linhas, banco tem ${CUR_ROWS} (queda >20%). Abortando sem tocar no banco."
  exit 1
fi

echo "--- update_database.R --replace"
"$RSCRIPT" update_database.R "e${ANO}.rds" "$ANO" --replace

echo "--- totais (gasto geral por candidato, denominador do KPI de %)"
TSE_ZIP_PATH="$ZIP" "$RSCRIPT" load_totais.R "$ANO"
"$RSCRIPT" update_totais.R "totais_${ANO}.rds" "$ANO"

# Compacta o WAL; leituras do app continuam funcionando normalmente
if [[ -n "$SQLITE" ]]; then
  "$SQLITE" "$DB" "PRAGMA wal_checkpoint(TRUNCATE);"
fi

# Só registre a revisão depois que todo o replace/totais/checkpoint terminar.
# Uma falha conserva a revisão anterior e força nova tentativa no próximo cron.
STATE_TMP="$(mktemp "$LOG_DIR/.etl_last_successful_code.XXXXXX")"
printf '%s\n' "$CODE_REV" > "$STATE_TMP"
mv -f "$STATE_TMP" "$CODE_STATE"

echo "=== OK $(date '+%Y-%m-%d %H:%M:%S') ==="
