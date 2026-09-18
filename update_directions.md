# Data Update Instructions

## Architecture

```
TSE zip (CSV) → load_data_<ANO>.R → update_database.R → despesas table (obseleitoral.db) → app.R
```

- **Database**: SQLite (`obseleitoral.db`), WAL mode
- **Table**: `despesas` — every election year in one table, distinguished by `ANO_ELEICAO`
- **App**: reads directly from SQLite through a connection pool (`pool::dbPool`)

## Adding a new election year (e.g. 2026)

```bash
# 1. Download the zip from a normal browser/network (cdn.tse.jus.br blocks datacenter
#    traffic), then either drop it at ./temp.zip or point TSE_ZIP_PATH at it:
TSE_ZIP_PATH=/path/to/prestacao_de_contas_eleitorais_candidatos_2026.zip Rscript load_data_2026.R

# 2. Load into the database (de-dupes on SQ_DESPESA within that year)
Rscript update_database.R e2026.rds 2026

# 2b. Totais de campanha (denominador do KPI de % de impulsionamento) - mesmo zip
TSE_ZIP_PATH=/path/to/prestacao_de_contas_eleitorais_candidatos_2026.zip Rscript load_totais.R 2026
Rscript update_totais.R totais_2026.rds 2026

# 3. Rebuild indexes if you ran anything outside update_database.R
sqlite3 obseleitoral.db < create_indexes.sql

# 4. Validate
Rscript validate_data_accuracy.R
```

To reload a year from scratch instead of appending: `Rscript update_database.R e2026.rds 2026 --replace`.

## Daily ETL (2026 campaign season — roda no container RStudio do CapRover)

`daily_etl.sh` runs the whole pipeline unattended: download → `load_data_2026.R` →
`update_database.R --replace` → `load_totais.R`/`update_totais.R` → WAL checkpoint. The Shiny
app reads the same `.db` from a shared CapRover volume, so updating the file IS the deploy.

- **Download**: `cdn.tse.jus.br` (Akamai) blocks curl/httr by **TLS fingerprint**, even from
  residential networks — plain `curl`/`httr::GET` get 403 no matter the User-Agent. The script
  uses `curl_cffi` (python) impersonating Chrome's TLS. One-time setup in the RStudio
  container: `pip3 install curl_cffi` (bake it into the app's Dockerfile so redeploys keep it).
  **Test from the server before scheduling** — datacenter IPs may be blocked on top of the
  fingerprint check:
  `python3 -c "from curl_cffi import requests; print(requests.head('https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_2026.zip', impersonate='chrome').status_code)"`
  If it prints 403, download the zip elsewhere and run the loaders with `TSE_ZIP_PATH=`.
- **`--replace`, not append**: TSE ships a full snapshot daily; replacing the year picks up
  amendments and deletions that append-with-dedup would miss. Replaces are transactional, so
  the app never sees a half-written year.
- **Guards**: stops early (no DB write) only when the snapshot is identical to what's in the DB
  (rows + total R$) **and** the same Git revision already completed an ETL. A newer revision
  reprocesses an unchanged snapshot so classifier/loader fixes reach SQLite; it aborts without
  touching the DB if the snapshot shrank >20% (broken file).
- **Env vars**: `OBS_DB_PATH` (where the shared `.db` lives; default `./obseleitoral.db`),
  `ETL_LOG_DIR` (default `<repo>/logs`).
- **Scheduling**: cron inside the RStudio container, e.g.
  `30 9 * * * /home/rstudio/observatorio-impulsionamento/daily_etl.sh`
  (TSE regenerates the zip early morning; 09:30 BRT is safe). `daily_etl.sh` itself does
  `git pull --ff-only` before downloading, so the persisted cron line remains valid after
  classifier/loader changes. The cron entry itself must still be baked into the image (or
  re-added after a redeploy) because a hand-added crontab dies with the container.

## CSV Format Requirements

Same shape TSE has used for candidate-expense filings since at least 2018 (see
`expected_cols` in `load_data_2026.R`). If TSE's new **Conta+JE** system renamed columns,
`load_data_2026.R` will stop with a clear error naming what's missing — update the column
list there before proceeding.

## Picking the right TSE dataset

- ✅ `prestacao_de_contas_eleitorais_candidatos_<ANO>.zip` — candidate-level expenses,
  what this app needs.
- ❌ `prestacao_contas_anual_partidaria_<ANO>.zip` — annual party accounting, party-level,
  no per-expense supplier detail. Do not use for this app.

## Notes

- Election-year filings are partial until after the election — partial accounts are due a
  few days before the 1st round, final accounts come after any runoff.
- 2022 data (`ANO_ELEICAO = 2022`) has no `mun_uf`/`rede_social_mae` — the original 2022 ETL
  didn't collect municipality or derive the platform column. Selecting a specific município
  or rede excludes 2022 rows for that reason.
