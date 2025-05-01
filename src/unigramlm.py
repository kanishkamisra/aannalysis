import re
import csv
import numpy as np
from transformers import AutoTokenizer


class UnigramLM:
    def __init__(self, counts_path):
        self.counts_path = counts_path
        self.model_name = counts_path.split("/")[-1].split(".")[0]
        try:
            self.tokenizer = AutoTokenizer.from_pretrained(
                f"kanishka/smolm-autoreg-bpe-{self.model_name}-1e-3"
            )
        except:
            self.tokenizer = AutoTokenizer.from_pretrained(
                f"kanishka/smolm-autoreg-bpe-{self.model_name}-1e-4"
            )

    def load_counts(self):
        self.counts = {}
        with open(self.counts_path, "r") as f:
            reader = csv.DictReader(f)
            for line in reader:
                self.counts[line["word"]] = int(line["count"])
        self.total_counts = sum(self.counts.values()) + len(self.counts.keys())

    def sentence_log_prob(self, sentence, token_wise=False):
        words = self.tokenizer.tokenize(sentence)
        probs = []
        for word in words:
            if word in self.counts:
                probs.append((self.counts[word] + 1) / self.total_counts)
            else:
                probs.append(1 / self.total_counts)

        if token_wise:
            return [np.log10(prob) for prob in probs]
        else:
            return np.mean([np.log10(prob) for prob in probs])
