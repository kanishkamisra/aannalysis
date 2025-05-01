#!/bin/bash

# readarray -t models < <( ls models/unigrams/ ); IFS=' '

# declare -a models=(counterfactual-babylm-old_union_new_regex_aanns_removal.csv counterfactual-babylm-new_regex_aanns_removal.csv)
# declare -a models=(counterfactual_babylm_naans_new counterfactual_babylm_300_naans_new counterfactual_babylm_300_anans_new)
# declare -a models=(counterfactual_babylm_anans_new)
# declare -a models=(counterfactual_babylm_measure_nps_as_singular_new counterfactual_babylm_aann_dtanns counterfactual_babylm_indef_articles_with_pl_nouns_removal_new counterfactual-babylm-new_regex_aanns_removal)
# # declare -a models=(counterfactual_babylm_naans_new-1e-3 counterfactual_babylm_300_naans_new-1e-3 counterfactual_babylm_300_anans_new-1e-3 counterfactual_babylm_anans_new-1e-3)

# declare -a models=(counterfactual-babylm-measure_nouns_as_singular.csv)
declare -a models=(counterfactual_babylm_anans_new.csv counterfactual_babylm_naans_new.csv)

for model in ${models[@]}
do 
    # python src/ngram-slors.py -m models/unigrams/${model} --ngram 1
    python src/ngram-slors.py -m models/fourgrams/${model} --ngram 4 -u models/unigrams/${model}
done

# declare -a variability=(high low)
# declare -a types=(all numeral adj noun)

# for variability in ${variability[@]}
# do 
#     for type in ${types[@]}
#     do
#         python src/ngram_tokenized_acceptability.py -m models/unigrams/counterfactual_babylm_aann_${variability}_variability_${type}.csv --ngram 1&
#     done
# done
# wait


# readarray -t models < <( ls models/fourgrams/ ); IFS=' '
# declare -a models=(counterfactual-babylm-measure-nouns-as-singular_removal.binary)
# declare -a models=(counterfactual-babylm-only_random_removal.binary)
# declare -a models=(counterfactual-babylm-only_indef_articles_with_pl_nouns_removal counterfactual-babylm-only_other_det_removal counterfactual-babylm-only_random_removal counterfactual-babylm-only_measure_nps_as_singular_removal)

# for model in ${models[@]}
# do 
#     # python src/ngram_tokenized_acceptability.py -m models/bigrams/${model} --ngram 2
#     python src/ngram_tokenized_acceptability.py -m models/fourgrams/${model} --ngram 4
# done