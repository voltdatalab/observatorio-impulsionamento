# CLAUDE.md

Guidance for Claude Code when working in this repository.

## What this is

**Observatório de Impulsionamento** — a Shiny app published by Núcleo Jornalismo that tracks
declared spending on social media ads (Meta, Google/YouTube, ByteDance/TikTok, Kwai) by
candidates in Brazilian elections, sourced from TSE campaign-finance filings ("prestação de
contas eleitorais"). Live at https://nucleojor.shinyapps.io/observatorio_impulsionamento/.

This is a single-app repo, not a multi-project workspace — everything here belongs to this app.

## Architecture

```
TSE zip (CSV) → load_data_YYYY.R (filter + tag) → despesas table in obseleitoral.db (SQLite)
                                                          │
                                                          ▼
                                                app.R (Shiny, reads via `pool`)
```

- **`app.R`** — the entire app (UI + server). `shinydashboard` skin with the sidebar/header
  hidden (see `www/custom.css`), so visually it does not look like a dashboard.
- **`obseleitoral.db`** — SQLite database, opened through a `pool::dbPool` connection pool.
  Filtering happens in SQL (`build_where_clause()`), not in R/dplyr — the app never loads the
  full table into memory. Candidate name search is server-side selectize (queries on keystroke,
  debounced).
- **The app always filters on exactly one election year** — the year selector has no "all
  years" option and defaults to the most recent `ANO_ELEICAO` (editorial decision, Sep 2026:
  summing nominal R$ across election years ignores inflation and misleads). Keep it that way;
  the unified table exists for storage/code simplicity, not for cross-year aggregation.
- **Main table: `despesas`** — one row per declared expense, all election years unioned together
  with an `ANO_ELEICAO` column (was one table per year — `e2024`, `e2022` — before the 2026
  update; kept as one table so year-over-year comparison and future years don't require new code
  paths). Key columns:
  - `NM_CANDIDATO`, `SG_PARTIDO`, `DS_CARGO`, `SG_UF`, `NM_UE` (município) — raw TSE fields
  - `DT_DESPESA` (dd/mm/yyyy, as TSE ships it) / `dt_despesa_iso` (YYYY-MM-DD, computed, indexed —
    always filter on this one, not `DT_DESPESA`)
  - `VR_DESPESA_CONTRATADA` (TEXT, Brazilian `1.234,56` format) / `valor_numeric` (REAL, computed,
    indexed — always use this one for filtering/math)
  - `mun_uf` — computed `"MUNICÍPIO - UF"` string, indexed, what the município dropdown/filter use
  - `rede_social_mae` — Meta / Google / ByteDance / Kwai / "Não informado", derived during ETL
    from supplier-name pattern matching (see `load_data_2024.R`) — **not present for 2022 rows**
    (older ETL didn't compute it; those rows are NULL and are excluded whenever a specific
    network filter is applied)
  - `SQ_DESPESA` — TSE's unique expense id, used as the de-dup key on updates
  - `ANO_ELEICAO` — election year (2022 / 2024 / 2026 / …), added for the unified table

- **Second table: `totais`** — gasto TOTAL de campanha agregado por candidato×turno (todas as
  despesas do CSV do TSE, não só impulsionamento), colunas de recorte iguais às de `despesas`
  (`ANO_ELEICAO`, `SG_UF`, `mun_uf`, `SG_PARTIDO`, `DS_CARGO`, `ST_TURNO`, `NM_CANDIDATO`) +
  `total_geral`. É o denominador do KPI "% do total gasto pelas campanhas foi com
  impulsionamento". Gerada por `load_totais.R <ANO>` (mesmo zip do TSE) e carregada com
  `update_totais.R` (sempre replace-por-ano). O % usa só filtros de universo de candidato —
  datas/rede/valor mínimo não se aplicam a um agregado por candidato.
- **`www/`** — `custom.css` (all app styling; brand colors `#4b31dd` purple, `#0fb872` green,
  `#FF8C42` chart orange; Barlow font from Google Fonts) and `resizer.js` (posts the iframe's
  height to the parent page — this app is embedded via iframe on nucleo.jor.br, so don't remove
  it even though nothing in `app.R` looks like it depends on it).

## Data pipeline / updating for a new election

1. Get the TSE zip. **Two different TSE datasets look similar — use the right one:**
   - ✅ `prestacao_de_contas_eleitorais_candidatos_<ANO>.zip` (candidate campaign expenses,
     per-expense-line, has supplier names) — this is what this app needs.
   - ❌ `prestacao_contas_anual_partidaria_<ANO>.zip` (annual party accounting, party-level,
     no per-candidate expense/supplier detail) — wrong dataset, will not fit this schema.
   - `cdn.tse.jus.br` blocks by **TLS fingerprint** (Akamai 403 for curl/httr/requests even
     from residential IPs, any User-Agent). Working download: `curl_cffi` with
     `impersonate="chrome"` — see the download step in `daily_etl.sh`.
   - Election-year filings are necessarily incomplete until after the election (partial accounts
     are due ~5 days before the 1st round; full/final accounts come after runoffs and any
     post-election audits).
2. Run `load_data_<ANO>.R` — downloads/reads the zip, filters rows to social-platform suppliers
   (`FACEBOOK|YOUTUBE|INSTAGRAM|TIKTOK|BYTEDANCE|GOOGLE|KWAI|META|WHATSAPP|JOYO TECNOLOGIA`, plus
   the "Despesa com Impulsionamento de Conteúdos" description), derives `rede_social_mae`, tags
   `ANO_ELEICAO`, writes an `.rds`.
3. Run `Rscript update_database.R e<ANO>.rds despesas` (appends; de-dupes on `SQ_DESPESA`) or
   `--replace` to reload a year from scratch. See `update_directions.md`.
4. `sqlite3 obseleitoral.db < create_views_and_indexes.sql` if indexes need rebuilding, then
   `ANALYZE`.
5. `Rscript validate_data_accuracy.R` — sanity-checks row counts/totals against the source file.

Party/platform name matching (`str_detect` on supplier name fields) is fragile by nature — TSE
supplier names are self-declared and inconsistent. Check `DS_ORIGEM_DESPESA`/`NM_FORNECEDOR`
values after each new load for new spellings or new platforms (e.g. X/Twitter reappearing) that
the regex should catch but doesn't.

## Performance notes

- Filtering must stay in SQL (`build_where_clause()` + the computed/indexed columns above) — the
  app's speed comes entirely from never materializing the full table in R.
- The `dados()` reactive is `debounce(500)`'d; every output reads from it, so it only re-queries
  once per settled filter change, not once per output.
- Don't recreate indexes you're not sure are used — a past optimization pass left ~2x duplicate
  indexes (same columns, two different naming conventions) plus several unused materialized
  tables (`vw_stats_by_*`, `daily_aggregations`, `summary_aggregations`, `processed_data`) and an
  unused FTS5 virtual table sitting in the `.db` file, together roughly doubling its size for no
  benefit — `app.R` never queried any of them. Before adding a new index or aggregation table,
  grep `app.R` first to confirm something will actually read it.
- Keep `PRAGMA journal_mode=WAL` set on the database file (better for shinyapps.io's concurrent
  sessions than the default rollback journal).

## Deployment (CapRover, desde set/2026)

- **App**: https://shinyverse.caprover.nucleotech.voltdata.info/observatorio-impulsionamento
  (container `shinyverse` no CapRover próprio). **ETL**: roda no container `rstudio`
  (https://rstudio.caprover.nucleotech.voltdata.info) via cron chamando `daily_etl.sh`.
- **Como os dados chegam ao app**: os dois containers montam o MESMO diretório persistente do
  host (CapRover → App Configs → Persistent Directories, mesmo host path nos dois apps). O ETL
  escreve `obseleitoral.db` nesse volume; o Shiny lê o mesmo arquivo. Nada de deploy por dados —
  `rsconnect`/shinyapps.io não são mais usados (o `.dcf`/`.rscignore` remanescentes são legado).
- `OBS_DB_PATH` (env var) aponta app e scripts de ETL para o `.db` no volume; sem ela, todos
  usam `./obseleitoral.db` (o default serve quando o repo inteiro vive no volume compartilhado).
- Os replaces anuais em `despesas`/`totais` são transacionais e o banco fica em WAL — o app pode
  ler durante a escrita sem ver estado intermediário.
- O deploy do CÓDIGO do app (app.R, www/) é separado dos dados: git pull/cópia para o diretório
  que o shiny server serve (tipicamente no mesmo volume).
- Atenção: o servidor CapRover tem IP de datacenter — teste o download do TSE de lá antes de
  confiar no cron (a Akamai pode bloquear IP além de fingerprint TLS; ver update_directions.md).

## Legacy / unused files

- `funcoes.R` — leftover helper functions from an unrelated project ("Science Pulse" Twitter
  tooling). Not sourced by `app.R` (the `source()` call is commented out). Safe to ignore.
- `playground.R`, `test_db_connection.R` — scratch files, not part of the app.
