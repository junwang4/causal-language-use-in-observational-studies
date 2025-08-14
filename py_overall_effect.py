import os
import re
import pandas as pd
from py_latex_helpers import *

def gen_latex_for_overall_effects(cfg):
    df = pd.read_csv(cfg.file_model_coef)
    df.columns = 'est stderr z pval'.split()
    print(df)

    tex_out = r'''
\begin{tabular}{@{}r@{ }l@{}}
\toprule

\begin{tabular}[t]{@{}l@{ }r@{ }l@{}l@{}}
\multicolumn{4}{@{}l}{\textbf{Author country}} \\
_country_list_ 
%[.05in]

\iffalse
\midrule 
\multicolumn{5}{l}{\footnotesize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}} \\ [.02in]
\fi

\end{tabular} 
& 

\begin{tabular}[t]{@{}l@{ }r@{ }l@{}l@{}}

%GENDER_FIRST_BEGIN%
\multicolumn{4}{@{}l}{\textbf{First author gender}} \\
  Men & gender_firstM \\
  Unknown &  gender_firstU \\[.05in]
%GENDER_FIRST_END%

%GENDER_LAST_BEGIN%
\multicolumn{4}{@{}l}{\textbf{Last author gender}} \\
  Men & gender_lastM \\
  Unknown &  gender_lastU \\[.05in]
%GENDER_LAST_END%

\multicolumn{4}{@{}l}{\textbf{Obs. studies published} (log-scaled)} \\
 as first author & first_author_paper_cnt_log \\
 as last author &   last_author_paper_cnt_log \\[.03in]

\multicolumn{4}{@{}l}{\textbf{Team size} (log-scaled)} \\
 Num co-authors & num_author_log \\ [.03in]


\midrule 
\multicolumn{4}{@{}l}{\textbf{Control Variables}} \\ [.05in]

\multicolumn{4}{@{}l}{\textbf{Journal} (log-scaled)} \\
 SciMago journal rank & sjr_log\\ 
 Obs. studies pub. & journal_paper_cnt_log \\ [.05in]

\multicolumn{4}{@{}l}{\textbf{Study design}} \\
_study_design_list_ 
[.05in]

%\multicolumn{4}{l}{\textbf{1400+ MeSH terms} (see SI D)} \\
\multicolumn{4}{@{}l}{\textbf{Other terms beyond study design}} \\
\multicolumn{4}{l}{ 1400+ MeSH terms (see SI D), each} \\ 
\multicolumn{4}{l}{ associated with 100 or more papers} \\ 
[.05in]

\midrule 
\multicolumn{4}{@{}l}{\textbf{Random Effects}} \\ [.05in]
 \multicolumn{4}{l}{Journal ISSN} \\
 \multicolumn{4}{l}{Publication year} \\
 \multicolumn{4}{l}{Conclusion length (num of sentences)} \\ [.03in]

\end{tabular} \\

\iffalse 
\midrule 
\multicolumn{2}{l}{\textbf{Random Effects}: Journal ISSN, Publication year, Conclusion length} \\ [.03in]
%\multicolumn{2}{l}{\footnotesize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$} }
\fi

\bottomrule
\end{tabular}
    '''

    vars = '''sjr_log 
        first_author_paper_cnt_log 
        last_author_paper_cnt_log 
        journal_paper_cnt_log 
        num_author_log'''.split()

    vars_first_ = ['gender_firstI', 'gender_firstL', 'gender_firstM']
    vars_last_ = ['gender_lastI', 'gender_lastL', 'gender_lastM']
    vars_firstLastLIcombined_ = ['gender_firstU', 'gender_firstM', 'gender_lastU', 'gender_lastM']

    vars += vars_firstLastLIcombined_

    for var in vars:
        if tex_out.find(var)>=0:
            if var in df.index:
                e = df[df.index==var].iloc[0]
                mark = '***' if e.pval<0.001 else '**' if e.pval<0.01 else '*' if e.pval<0.05 else r''
                item = f'{e.est:.3f} & ({e.stderr:.3f}) & $^' + '{' + mark + '}$'
            tex_out = tex_out.replace(var, item)

    country_list_tex = []
    for factor in df.sort_values('est').index:
        if factor.startswith('countryc_'):
            country_code = factor.split('_')[-1]
            country_name = cfg.country_code2name[country_code]
            #country_name = re.sub(r',.*', '', country_name) # some country names have comma such as "Tanzania, United Republic of"
            e = df.loc[factor]
            mark = '***' if e.pval<0.001 else '**' if e.pval<0.01 else '*' if e.pval<0.05 else r''
            item = f' {country_name} & {e.est:.3f} & ({e.stderr:.3f}) & $^' + '{' + mark + '}$ \\\\'
            item += f' [-.022in]'
            country_list_tex.append(item)

    tex_out = tex_out.replace('_country_list_', '\n'.join(country_list_tex) )

    study_design_list_tex = []

    dt = pd.read_csv(cfg.file_data_meshterm, usecols=['term'])
    print(dt.shape)

    for factor in df.sort_values('est').index:
        if factor.startswith('t_'):
            tabbr_tfull = {re.sub(r'\W+', '', full): full for full in dt.term.unique() \
                            if full.find('Studies')>=0 and full.find('Feasibility')<0}
            factor_name = f'{factor.replace("t_", "")}'
            if factor_name in tabbr_tfull:
                factor_name = tabbr_tfull[factor_name]
                factor_name = factor_name.replace('&', '\\&')
                factor_name = factor_name.replace(' Studies', '')
                e = df.loc[factor]
                mark = '***' if e.pval<0.001 else '**' if e.pval<0.01 else '*' if e.pval<0.05 else r''
                #item = f' & {factor_name} & {e.est*100:.1f} & ({e.stderr*100:.1f}) & $^' + '{' + mark + '}$ \\\\'
                item = f'  {factor_name} & {e.est:.3f} & ({e.stderr:.3f}) & $^' + '{' + mark + '}$ \\\\'
                study_design_list_tex.append(item)

    tex_out = tex_out.replace('_study_design_list_', '\n'.join(study_design_list_tex) )

    print(tex_out)
    filename_out = f'overall_effects'
    export_tex_with_and_without_header_footer(cfg, tex_out, filename_out)
