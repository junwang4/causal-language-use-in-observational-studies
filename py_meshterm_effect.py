import re
import pandas as pd
from py_latex_helpers import *

def gen_latex_for_meshterm_longtable(cfg):
    symbol = 't_'

    print('\n- read csv from:', cfg.file_model_coef)
    df = pd.read_csv(cfg.file_model_coef)
    df.columns = 'est stderr z pval'.split()

    df = df[df.index.str.startswith('t_')]
    df = df.sort_values('est')

    dt = pd.read_csv(cfg.file_data_meshterm, usecols=['term'])
    symbol_str = 'Mesh terms'
    tabbr_tfull = {re.sub(r'\W+', '', full): full for full in dt.term.unique()}

    line_b = r'\begin{table}' \
             + r'\caption{' + symbol_str + r'} \vskip .2in ' \
             + r'\begin{tabular}{r' + 'r@{}l' + r'r @{ }}'

    line_e = r'\end{tabular}' \
             + r'\end{table} \newpage'

    line_b = r'\begin{longtable}[l]{r' + 'r@{}l' + r'@{ }}' \
    + r'\hline \\ '
            #     + r'\caption{' + symbol_str + r'} \\ '
    #line_e = r'\end{longtable} \pagebreak'
    line_e = r'[.05in] \hline \\[.05in] \caption{Effects of the 1,400+ MeSH terms.} \end{longtable}'

    out = [line_b]

    cnt = 0
    for i, factor in enumerate(df.index):
        if 1:
            factor_name = f'{factor.replace(symbol, "")}'
            if factor_name in tabbr_tfull:
                factor_name = tabbr_tfull[factor_name]
                factor_name = factor_name.replace('&', '\\&')
            else:
                continue
        else:
            factor_name = f'{factor.replace(symbol, "")}'

        ooo = [factor_name]
        e = df.loc[factor]
        #mark = '***' if e.pval<0.001 else '**' if e.pval<0.01 else '*' if e.pval<0.05 else r'\cdot' if e.pval<0.1 else ''
        mark = '***' if e.pval<0.001 else '**' if e.pval<0.01 else '*' if e.pval<0.05 else r''
        item = f'{e.est:.3f} ({e.stderr:.3f}) & $^' + '{' + mark + '}$'

        ooo.append(item)

        row = ' & '.join(ooo) + r'\\ '
        if i<len(df)-1:
            row += r'[-.012in]'
        out.append(row)
        cnt += 1

    out.append(line_e)
    result = '\n'.join(out)
    print(result)


    export_tex_with_and_without_header_footer(cfg, result, 'meshterm_effects')

    print('\n- cnt:', cnt)
