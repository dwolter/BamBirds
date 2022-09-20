from sklearn import preprocessing
from sklearn.naive_bayes import GaussianNB, MultinomialNB
from sklearn.feature_extraction.text import TfidfVectorizer
import pandas as pd
import numpy as np
from sklearn.pipeline import make_pipeline

import BOW as b


class NaiveBayes:
    def __init__(self, _features, _labels):
        self.model = GaussianNB()
        self.features = _features
        self.labels = _labels

    def train(self):
        # Train the model
        self.model.fit(self.features, self.labels)

    def predict(self, _instance):
        return self.model.predict(_instance)


if __name__ == "__main__":
    features = [[0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
                [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 4, 2, 3, 1, 1],
                [1, 0, 0, 0, 0, 2, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0],
                [0, 3, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 2, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]

    features_2 = [[0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
                  [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1],
                  [1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0],
                  [0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                  [0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                  [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                  [0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                  [0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                  [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0]
                  ]
    features_3 = ['02 05 05 12', '02', '04 05 08 12 12 12 12 13 13 14 14 14 15 16', '00 05 05 07 13', '01 01 01 06',
                  '01 01 06', '01 01 01 01']

    labels = ['bird', 'bird', 'bird', 'pig', 'ice', 'ice', 'ice', 'wood', 'wood', 'wood']
    labels_2 = ['bird', 'bird', 'bird', 'pig', 'ice', 'ice', 'ice']
    labelEncoder = preprocessing.LabelEncoder()
    labels = labelEncoder.fit_transform(labels)
    # print(labels)

    NB = NaiveBayes(features_2, labels)
    NB.train()

    instance = [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    instance_2 = ['04 05 08 12 12 12 12 13 13 14 14 14 15 16']

    instance = np.asarray(instance).reshape(1, -1)

    # print(NB.predict(instance))

    vocab_string = b.get_vocab_string()
    print(vocab_string)

    vec = TfidfVectorizer(analyzer='word', stop_words=None)
    X = vec.fit_transform(vocab_string)
    #print(pd.DataFrame(X.toarray(), columns=vec.get_feature_names()))
    model = make_pipeline(TfidfVectorizer(), MultinomialNB())
    model.fit(features_3, labels_2)
    # labels = model.predict(Sciencebirds_vision2020.data)

    #print(model.predict(instance_2))
