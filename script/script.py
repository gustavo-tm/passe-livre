import pandas as pd
import numpy as np
import basedosdados as bd
import glob 
import json

anos = "(2002, 2006, 2010, 2014, 2018, 2022)"

def abstencao():
    query = f'''
    SELECT id_municipio, ano, turno, aptos, abstencoes
    FROM `basedosdados.br_tse_eleicoes.detalhes_votacao_municipio` 
    WHERE ano IN {anos} AND cargo = 'presidente'
    '''

    (
    bd.read_sql(query, billing_project_id="python-371123")
    .dropna().astype(int)
    .to_csv("data/abstencao.csv")
    )

def competitividade():

    def calculate_k(turno):
        query = f'''
        SELECT id_municipio, numero_candidato, ano, votos, turno
        FROM `basedosdados.br_tse_eleicoes.resultados_candidato_municipio` 
        WHERE sigla_uf != 'ZZ' AND ano IN {anos} AND cargo = 'presidente' AND turno = {turno} AND (resultado != 'nao eleito' OR turno = 2)
        '''

        return (
        bd.read_sql(query, billing_project_id="python-371123")
        .dropna().astype(int)
        .assign(numero_candidato = lambda _: _.numero_candidato == 13)
        .pivot_table(index = ["id_municipio", "ano"], columns = ["numero_candidato"], values = "votos")
        .reset_index().rename_axis("", axis = 1)
        .assign(competitividade = lambda _: np.where(_[0] != _[1], 1/abs(np.log(_[0]/_[1])), np.nan),
                turno = turno)
        .filter(["id_municipio", "ano", "competitividade", "turno"])
        )
    
    (
    pd.concat([
        calculate_k(turno = 1), 
        calculate_k(turno = 2)])
    .to_csv("data/t-competitividade.csv")
    )

def ideb():
    query = '''
    SELECT id_municipio, ano, ideb
    FROM `basedosdados.br_inep_ideb.municipio` 
    WHERE ensino = 'fundamental' AND anos_escolares = 'finais (6-9)' AND rede = 'publica' AND ano IN (2001, 2005, 2009, 2013, 2017, 2021)
    '''

    (
        bd.read_sql(query, billing_project_id="python-371123")
        .assign(ano = lambda _: _.ano + 1)
        .to_csv("data/ideb.csv")
    )
    
def pib():
    query = '''
    SELECT id_municipio, pib, va_agropecuaria, va_industria, 
            va_servicos, va_adespss, ano
    FROM `basedosdados.br_ibge_pib.municipio` 
    WHERE ano IN (2002, 2006, 2010, 2014, 2018, 2020)
    '''

    pib = bd.read_sql(query, billing_project_id="python-371123")

    #Enquanto não saem os dados de 2022
    pib = pib.assign(ano = lambda _: np.where(_.ano == 2020, 2022, _.ano))

    pib.to_csv("data/pib.csv")

def populacao():
    query = f'''
    SELECT id_municipio, ano, populacao
    FROM `basedosdados.br_ibge_populacao.municipio` 
    WHERE ano IN (2002, 2006, 2010, 2014, 2018, 2021)
    '''

    populacao = bd.read_sql(query, billing_project_id="python-371123")
    
    #Enquanto não saem os dados de 2022
    populacao = populacao.assign(ano = lambda _: np.where(_.ano == 2021, 2022, _.ano))

    populacao.to_csv("data/populacao.csv")

def frota():
    query = f'''
    SELECT id_municipio, ano, total as veiculos
    FROM `basedosdados.br_denatran_frota.municipio_tipo` 
    WHERE ano IN (2002, 2006, 2010, 2014, 2018, 2020) AND mes = 10
    '''

    frota = bd.read_sql(query, billing_project_id="python-371123")
    
    #Enquanto não saem os dados de 2022
    frota = frota.assign(ano = lambda _: np.where(_.ano == 2020, 2022, _.ano))

    frota.to_csv("data/frota.csv")

def download_bd():
    """Efetua o download de todas as bases necessarias"""

    bases = [abstencao, competitividade, ideb, pib, populacao]
    
    for base in bases:
        print(str(base))
        base()

def controle_tratamento():
    df = pd.read_csv("script/passe_livre.csv", index_col = 0)
    (df
     .query("ano == 2022").assign(tratamento = lambda _: _.passe_livre)
     .filter(["id_municipio", "turno", "tratamento"])
     .merge(df, on = ["id_municipio", "turno"], how = "right")
     .to_csv("data/t-passe_livre.csv")
    )

def merge():
    """Junta todas as bases baixadas"""

    df = pd.read_csv("data/abstencao.csv", index_col=0)
    for base in [base for base in glob.glob("data/*") if base != "data\\abstencao.csv"]:
        print(base)
        
        if base[5:7] == "t-":
            merge_on = ["id_municipio", "ano", "turno"]
        else:
            merge_on = ["id_municipio", "ano"]

        df = pd.merge(df, pd.read_csv(base, index_col=0), on = merge_on, how = "left")
    
    contas = json.loads(open("script/variable_operations.json").read())
    variaveis = json.loads(open("script/variables.json").read()).keys()
    
    filtro = list(pd.read_csv("script/filter.csv").id_municipio.unique())

    (df
     .query(f"id_municipio not in {filtro}")
     .sort_values(["ano", "id_municipio", "turno"])
     .assign(**{variavel: eval(conta) for variavel, conta in contas.items()})
     .filter(variaveis)
     .to_csv("data.csv", index = False)
     )


# download_bd()
controle_tratamento()
merge()