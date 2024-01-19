
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Biogeographical history explains the spatial distribution of *Myrcia* diversity in the Atlantic Forest

<!-- badges: start -->
<!-- badges: end -->

Here you find code and data to reproduce the analysis

## Repository structure

    #> .
    #> ├── data
    #> │   ├── 000_areas_myrcia_phy_consensus.data
    #> │   ├── 000_phy_myrcia_cleaned_consensus.new
    #> │   ├── evoregions_stand_names_df.csv
    #> │   ├── list_results_models_biogeobears_phy_consenso.rds
    #> │   ├── res_evoregion_phy_consensus.rds
    #> │   ├── shape
    #> │   ├── W.csv
    #> │   └── W_xy.csv
    #> ├── fig
    #> │   ├── chart_insitu.png
    #> │   ├── disp_from.png
    #> │   ├── fig2_evoregions.svg
    #> │   ├── fig_2_evoregions.png
    #> │   ├── map_evo_af.png
    #> │   ├── map_insert_af.png
    #> │   └── map_insert_evo.png
    #> ├── LICENSE
    #> ├── myrcia_insitu_div.Rproj
    #> ├── output
    #> │   ├── div_age_df.rds
    #> │   ├── evo_metrics_df.rds
    #> │   ├── myrcia_disp.rds
    #> │   └── ses_mpd_myrcia.rds
    #> ├── README.md
    #> ├── README.Rmd
    #> └── script
    #>     ├── 99_01_extra_sink_source_areas.R
    #>     ├── 99_02_extra_figs.R
    #>     ├── do
    #>     └── fig

All data in `data` folder (except the ones in `data/shape`) was copied
from the repo <https://github.com/avrodrigues/myrcia_hist_biogeography>
(\[DOI: 10.5281/zenodo.10377169\]
(<https://zenodo.org/records/10377169>)) and was used before in the
analysis of biogeogaphy history of Myrcia in the paper Rodrigues and
Duarte 2023 [Mapping species richness and evolutionary regions of the
genus Myrcia](https://doi.org/10.1111/jbi.14791)
