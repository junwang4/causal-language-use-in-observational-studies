
header = r'''
\documentclass[10pt,letterpaper]{article}
\usepackage{fullpage}
\usepackage{booktabs}
\usepackage{longtable}
\usepackage{multirow}

%\usepackage{float}
%\newcommand{\ttt}{\texttt\footnotesize}
\begin{document}
'''
footer = r'''
\end{document}
'''

def export_tex_with_and_without_header_footer(cfg, tex_out, fname):
    with open(f'{cfg.tmp_out_dir}/{fname}_full.tex', 'w') as fout:
        fout.write(header)
        fout.write(tex_out)
        fout.write(footer)
        print(f'\n- Saved to {fout.name}\n')

    with open(f'{cfg.tex_out_dir}/{fname}.tex', 'w') as fout:
        fout.write(tex_out)
        print(f'\n- Saved to {fout.name}\n')
