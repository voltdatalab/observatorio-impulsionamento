-- Rebuilds the index set on `despesas` (idempotent - safe to re-run any time).
-- Kept in sync with the create_indexes() function in update_database.R, which
-- runs the same statements automatically after every data load.

CREATE INDEX IF NOT EXISTS idx_despesas_ano ON despesas(ANO_ELEICAO);
CREATE INDEX IF NOT EXISTS idx_despesas_dt_despesa_iso ON despesas(dt_despesa_iso);
CREATE INDEX IF NOT EXISTS idx_despesas_sg_partido ON despesas(SG_PARTIDO);
CREATE INDEX IF NOT EXISTS idx_despesas_sg_uf ON despesas(SG_UF);
CREATE INDEX IF NOT EXISTS idx_despesas_ds_cargo ON despesas(DS_CARGO);
CREATE INDEX IF NOT EXISTS idx_despesas_st_turno ON despesas(ST_TURNO);
CREATE INDEX IF NOT EXISTS idx_despesas_sq_despesa ON despesas(SQ_DESPESA);
CREATE INDEX IF NOT EXISTS idx_despesas_sq_candidato ON despesas(SQ_CANDIDATO);
CREATE INDEX IF NOT EXISTS idx_despesas_rede_social ON despesas(rede_social_mae);
CREATE INDEX IF NOT EXISTS idx_despesas_nm_candidato ON despesas(NM_CANDIDATO);
CREATE INDEX IF NOT EXISTS idx_despesas_nm_ue ON despesas(NM_UE);
CREATE INDEX IF NOT EXISTS idx_despesas_mun_uf ON despesas(mun_uf);
CREATE INDEX IF NOT EXISTS idx_despesas_valor_numeric ON despesas(valor_numeric);
CREATE INDEX IF NOT EXISTS idx_despesas_uf_mun ON despesas(SG_UF, NM_UE);
CREATE INDEX IF NOT EXISTS idx_despesas_partido_cargo ON despesas(SG_PARTIDO, DS_CARGO);
CREATE INDEX IF NOT EXISTS idx_despesas_date_partido ON despesas(dt_despesa_iso, SG_PARTIDO);
CREATE INDEX IF NOT EXISTS idx_despesas_partido_valor ON despesas(SG_PARTIDO, valor_numeric);
CREATE INDEX IF NOT EXISTS idx_despesas_uf_valor ON despesas(SG_UF, NM_UE, valor_numeric);
CREATE INDEX IF NOT EXISTS idx_despesas_candidato_partido ON despesas(NM_CANDIDATO, SG_PARTIDO, valor_numeric);
CREATE INDEX IF NOT EXISTS idx_despesas_ano_partido ON despesas(ANO_ELEICAO, SG_PARTIDO);

PRAGMA journal_mode=WAL;
ANALYZE;
