#!/bin/bash

# readarray -t models < <( ls models/tokenized/ ); IFS=' '
# declare -a models=(counterfactual_babylm_aann_dtanns.txt counterfactual_babylm_measure_nps_as_singular_new.txt counterfactual_babylm_indef_articles_with_pl_nouns_removal_new.txt counterfactual-babylm-new_regex_aanns_removal.txt)
# declare -a models=(counterfactual-babylm-only_indef_articles_with_pl_nouns_removal counterfactual-babylm-only_other_det_removal counterfactual-babylm-only_random_removal counterfactual-babylm-only_measure_nps_as_singular_removal)

declare -a models=(counterfactual_babylm_anans_new.txt counterfactual_babylm_naans_new.txt)

for model in ${models[@]}
do
    models/kenlm/build/bin/lmplz -o 4 -S 40% -T models/kenlm/build/tmp <models/tokenized/${model} >models/fourgrams/${model}.arpa
    
    models/kenlm/build/bin/build_binary -T models/kenlm/tmp/ -S 40% trie models/fourgrams/${model}.arpa models/fourgrams/${model}.binary
    
    rm models/fourgrams/${model}.arpa
done
