import argparse
import inflect
import os
import pathlib
import utils
import kenlm

import numpy as np
import pandas as pd

from collections import defaultdict
from tqdm import tqdm
from functools import reduce
from unigramlm import UnigramLM
from transformers import AutoTokenizer


def compose(*functions):
    """compose functions"""
    return reduce(lambda f, g: lambda x: f(g(x)), functions, lambda x: x)


inflector = inflect.engine()


def main(args):

    ngram2dir = {
        1: "unigrams",
        2: "bigrams",
        3: "trigrams",
        4: "fourgrams",
    }

    os.environ["TOKENIZERS_PARALLELISM"] = "false"
    model_name = args.model

    model_name = model_name.split("/")[-1].split(".")[0]

    aann_dir = args.aann_dir.split("data")[-1].strip("/")

    # load model
    if args.ngram == 1:
        lm = UnigramLM(args.model)
        lm.load_counts()
        pathlib.Path(args.results_dir).mkdir(parents=True, exist_ok=True)
        pathlib.Path(f"{args.results_dir}/unigrams-alt").mkdir(parents=True, exist_ok=True)
    else:
        unigram_lm = UnigramLM(args.unigram)
        unigram_lm.load_counts()

        ngram_lm = args.model.replace(".csv", ".txt.binary")

        lm = kenlm.Model(ngram_lm)
        try:
            tokenizer = AutoTokenizer.from_pretrained(
                f"kanishka/smolm-autoreg-bpe-{model_name}-1e-3"
            )
        except:
            tokenizer = AutoTokenizer.from_pretrained(
                f"kanishka/smolm-autoreg-bpe-{model_name}-3e-4"
            )
        pathlib.Path(args.results_dir).mkdir(parents=True, exist_ok=True)
        pathlib.Path(f"{args.results_dir}/{ngram2dir[args.ngram]}-alt").mkdir(
            parents=True, exist_ok=True
        )

        def n_gram_slor(prefix, continuation):
            unigram_logprob = unigram_lm.sentence_log_prob(" " + continuation)

            full = f"{prefix} {continuation}"
            full_tokenized = tokenizer.tokenize(" " + full)
            # print(full_tokenized)
            full_length = len(full_tokenized)

            scores = list(lm.full_scores(" ".join(full_tokenized)))

            prefix_tokenized = tokenizer.tokenize(" " + prefix)
            # print(prefix_tokenized)
            prefix_len = len(prefix_tokenized)

            region = [p[0] for p in scores][prefix_len:-1]

            return (np.sum(region) / (full_length - prefix_len)) - unigram_logprob

    constructions = utils.read_csv_dict(f"{args.aann_dir}/corruption.csv")

    results = defaultdict(list)
    results["idx"] = [c["idx"] for c in constructions]

    columns = [
        "construction",
        "default_nan",
        "order_swap",
        "no_article",
        "no_modifier",
        "no_numeral",
    ]

    for construction in tqdm(constructions):
        for col in columns:
            # print(f"{col}: {construction['prefix'] +  ' ' + construction[col]}")
            if args.ngram == 1:
                score = lm.sentence_log_prob(" " + construction[col])
            else:
                # tokenized = tokenizer.tokenize(" " + construction[col])
                # scores = [p[0] for p in list(lm.full_scores(" ".join(tokenized)))[1:-1]]
                # score = np.sum(scores) / len(tokenized)
                score = n_gram_slor(construction['prefix'], construction[col])

            results[f"{col}_score"].append(score)

    results = dict(results)
    results = pd.DataFrame(results)
    results.to_csv(
        f"{args.results_dir}/{ngram2dir[args.ngram]}-alt/{model_name}.csv", index=False
    )
    # if args.unigram:
    #     results.to_csv(f"{args.results_dir}/unigrams/{model_name}.csv", index=False)
    # else:
    #     results.to_csv(f"{args.results_dir}/bigrams/{model_name}.csv", index=False)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--aann_dir",
        "-a",
        type=str,
        default="data/mahowald-aann/",
        help="path to aanns or their counterfactuals",
    )
    parser.add_argument(
        "--model",
        "-m",
        type=str,
        default="models/babylm.csv",
        help="path to model",
    )
    parser.add_argument(
        "--unigram",
        "-u",
        type=str,
        default="models/unigrams/babylm.csv"
    )
    parser.add_argument(
        "--results_dir",
        type=str,
        default="results/",
        help="path to results directory",
    )
    parser.add_argument(
        "--ngram",
        type=int,
        default=1,
        help="path to results directory",
    )
    args = parser.parse_args()

    main(args)
