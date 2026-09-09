# Observatório de Impulsionamento

Shiny app tracking declared social-media ad spending (Meta, Google/YouTube, TikTok, Kwai) by
candidates in Brazilian elections, sourced from TSE campaign-finance filings.

Live at https://nucleojor.shinyapps.io/observatorio_impulsionamento/

## Run it

```bash
R -e "shiny::runApp('app.R')"
```

## Update data / architecture / deployment

See [`CLAUDE.md`](CLAUDE.md) for the full picture (data pipeline, database schema,
performance notes, deployment) and [`update_directions.md`](update_directions.md) for the
step-by-step to load a new election year.

```bash
Rscript validate_data_accuracy.R   # sanity-check the database against a source file
```

FONTE: TSE / Análise Núcleo Jornalismo
