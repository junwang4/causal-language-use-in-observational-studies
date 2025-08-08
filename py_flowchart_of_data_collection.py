import json
import pandas as pd

header = r'''
\documentclass{article}
\usepackage{tikz}
%\usepackage{adjustbox}
\begin{document}
'''
footer = r'''
\end{document}
'''

tex_out = r'''
\begin{tikzpicture}
    \tikzstyle{block} = [rectangle, rounded corners, draw, text width=28em, text centered, minimum height=3.5em]

    % Place nodes
    \node [block] (block1) {NUM_ALL_OBSERVATIONAL_STUDIES observational studies retrieved from the Entrez pubmed database
    using query {\tt Observational Study[Publication Type]} \\
    (up to CURR_DATE)};
    
    \node [block, below of=block1, yshift=-1.8cm] (block2) {
        %NUM_EXCLUDING_TRIALS exclusive or non-interventional observational studies
        NUM_EXCLUDING_TRIALS exclusive observational studies
    };
    
    \node [block, below of=block2, yshift=-1.5cm] (block2_3) {
        NUM_STRUCTURED_ABSTRACT have a structured abstract \\ 
    };
    \node [block, below of=block2_3, yshift=-1.5cm] (block3) {
        NUM_CONCLUSION papers include a subsection of {\em Conclusion(s)} \\ in their {\em structured abstracts}
    };
    \node [block, below of=block3, yshift=-1.5cm] (block4) {
        NUM_ENGLISH are written in English in their full-text
    };
    \node [block, below of=block4, yshift=-1.5cm] (block5) {
        NUM_FINAL papers used in our regression analysis
    };

    \draw[->] (block1) -- (block2) node[anchor=west, midway, align=left] {
        Exclude papers that are labeled as \\ Randomized Controlled Trial or Clinical Trial
    };
    
    \draw[->] (block2) -- (block2_3) node[anchor=west, midway, align=left] {
        Exclude papers that do not have structured abstract 
    };

    \draw[->] (block2_3) -- (block3) node[anchor=west, midway, align=left] {
        Exclude papers that do not have a \\ conclusion subsection in the abstract
    };

    \draw[->] (block3) -- (block4) node[anchor=west, midway, align=left] {
        Exclude papers that are not written in English \\ in their full-text
    };

    \draw[->] (block4) -- (block5) node[anchor=west, midway, align=left] {
        Exclude papers that do not contain any causal or \\
        associational claims in the conclusion 
    };

\end{tikzpicture}
'''

def gen_tikz_flowchart_tex(cfg):
    global tex_out

    columns = '''CURR_DATE NUM_ALL_OBSERVATIONAL_STUDIES NUM_EXCLUDING_TRIALS NUM_STRUCTURED_ABSTRACT NUM_CONCLUSION NUM_ENGLISH NUM_FINAL'''.split()

    # {"CURR_DATE": "2024-01-01", "NUM_ALL_OBSERVATIONAL_STUDIES": 150497, "NUM_BETWEEN_2013_AND_2023": 149664, "NUM_EXCLUDING_TRIALS": 145474, "NUM_CONCLUSION": 103086, "NUM_ENGLISH": 97293}
    col_val = json.load(open(cfg.file_flowchart_data_statistics))

    col_val['NUM_FINAL'] = len(pd.read_csv(cfg.file_data_main))
    print(col_val)

    for col in columns:
        print(col, col_val[col])
        val = col_val[col]
        if col != 'CURR_DATE':
            val = f'{val:,}'
        tex_out = tex_out.replace(col, val)

    with open(f'{cfg.tmp_out_dir}/flowchart_data_collection_full.tex', 'w') as fout:
        fout.write(header)
        fout.write(tex_out)
        fout.write(footer)
        print(f'\n- Saved to {fout.name}\n')

    with open(f'{cfg.tex_out_dir}/flowchart_data_collection.tex', 'w') as fout:
        fout.write(tex_out)
        print(f'- Saved to {fout.name}\n')
