import argparse
from dataclasses import dataclass, field
import os
import pandas as pd

from py_flowchart_of_data_collection import gen_tikz_flowchart_tex
from py_overall_effect import gen_latex_for_overall_effects
from py_meshterm_effect import gen_latex_for_meshterm_longtable

tmp = pd.read_csv(f'csv/UAI.csv', usecols=['country', 'country_code'])
country_code2name = {code:name for code, name in zip(tmp.country_code, tmp.country)}

@dataclass
class Config:
    task: str
    tmp_out_dir: str
    tex_out_dir: str
    pdf_out_dir: str
    file_model_coef: str
    file_data_main: str = 'csv/data_main.csv'
    file_data_meshterm: str = 'csv/data_meshterm.csv'
    file_flowchart_data_statistics: str = 'csv/flowchart_data_statistics.json'
    meshterm_min_paper_cnt: int = 100
    gender_min_confidences: str = 'M82F78'
    country_code2name: dict = field(default_factory=lambda: country_code2name.copy())

def parse_args():
    parser = argparse.ArgumentParser(description="Generate tables and charts for presentation results.")
    
    parser.add_argument("--task", required=True, type=str,
                        help="Task name, e.g., 'overall_effect'")
    parser.add_argument("--meshterm_min_paper_cnt", required=True, type=int,
                        help="Threshold value for mesh term filtering")
    parser.add_argument("--gender_min_confidences", required=True, type=str,
                        help="Gender tag to filter results")

    return parser.parse_args()

def main():
    args = parse_args()

    output_dir = "working/report"
    tex_out_dir = f'{output_dir}/tex'
    pdf_out_dir = f'{output_dir}/pdf'
    tmp_out_dir = f'{output_dir}/tmp'
    file_model_coef = f'working/model/coef_journalT_meshtermT_meshterm{args.meshterm_min_paper_cnt}_gender{args.gender_min_confidences}.csv'

    if not os.path.exists(tex_out_dir):
        os.makedirs(tex_out_dir)
    if not os.path.exists(pdf_out_dir):
        os.makedirs(pdf_out_dir)
    if not os.path.exists(tmp_out_dir):
        os.makedirs(tmp_out_dir)

    cfg = Config(
        task=args.task,
        tmp_out_dir=tmp_out_dir,
        tex_out_dir=tex_out_dir,
        pdf_out_dir=pdf_out_dir,
        meshterm_min_paper_cnt=args.meshterm_min_paper_cnt,
        gender_min_confidences=args.gender_min_confidences,
        file_model_coef=file_model_coef
    )

    # Example logic using cfg
    print("Task:", cfg.task)
    print("Mesh term threshold:", cfg.meshterm_min_paper_cnt)
    print("Gender min confidences:", cfg.gender_min_confidences)

    if cfg.task == "overall_effect":
        gen_latex_for_overall_effects(cfg)

    elif cfg.task == "meshterm_effect":
        gen_latex_for_meshterm_longtable(cfg)

    elif cfg.task == "flowchart_data_collection":
        gen_tikz_flowchart_tex(cfg)

if __name__ == "__main__":
    main()