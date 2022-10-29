from operator import length_hint
from time import sleep
import nltk
from epc.server import EPCServer
from epc.client import EPCClient
from threading import Thread
from numpy import append
from scipy import rand
from scipy.optimize import root

from sexpdata import parse

# GRAMMAR = '''
#     Subject: {<NNP><NNP>}
#     SubjectInfo: {<CD><NNS><JJ>}
#     Action: {<MD><VB>}
#     Object: {<DT><NN>}
#     Stopwords: {<IN><DT>}
#     ObjectInfo: {<JJ><NN>}
#     When: {<NNP><CD>}
# '''

GRAMMAR = "MY_NP: {<DT|CD|PRP\$>?<JJ>*<NN|NNS>}"


server = EPCServer(("localhost", 0))
client = ""

NLTK_CLASS = {
    "CC": ["Coordinating conjunction","并列连词"],
    "CD": ["Cardinal number","基数词"],
    "DT": ["Determiner","限定符"],
    "EX": ["Existential there","存在词"],
    "FW": ["Foreign word","外来词"],
    "IN": ["Preposition or subordinating conjunction","介词或从属连词"],
    "JJ": ["Adjective","形容词"],
    "JJR": ["Adjective, comparative","比较级的形容词"],
    "JJS": ["Adjective, superlative","最高级的形容词"],
    "LS": ["List item marker","列表项标记"],
    "MD": ["Modal","情态动词"],
    "NN": ["Noun, singular or mass","名词单数"],
    "NNS": ["Noun, plural","名词复数"],
    "NNP": ["Proper noun, singular","专有名词"],
    "NNPS": ["Proper noun, plural","专有名词复数"],
    "PDT": ["Predeterminer","前置限定词"],
    "POS": ["Possessive ending","所有格结尾"],
    "PRP": ["Personal pronoun","人称代词"],
    "PRP$": ["Possessive pronoun","所有格代词"],
    "RB": ["Adverb","副词"],
    "RBR": ["Adverb, comparative","副词比较级"],
    "RBS": ["Adverb, superlative","副词最高级"],
    "RP": ["Particle","小品词"],
    "SYM": ["Symbol","符号"],
    "TO": ["to","to"],
    "UH": ["Interjection","感叹词"],
    "VB": ["Verb, base form","动词原型"],
    "VBD": ["Verb, past tense","动词过去式"],
    "VBG": ["Verb, gerund or present participle","动名词或现在分词"],
    "VBN": ["Verb, past participle","动词过去分词"],
    "VBP": ["Verb, non-3rd person singular present","非第三人称单数的现在时"],
    "VBZ": ["Verb, 3rd person singular present","第三人称单数的现在时"],
    "WDT": ["Wh-determiner","以wh开头的限定词"],
    "WP": ["Wh-pronoun","以wh开头的代词"],
    "WP$": ["Possessive wh-pronoun","以wh开头的所有格代词"],
    "WRB": ["Wh-adverb","以wh开头的副词"],
}


def split_sentence(sentence):
    # nltk.download('averaged_perceptron_tagger')
    words = nltk.TreebankWordTokenizer().tokenize(sentence)
    spans = [span for span in nltk.TreebankWordTokenizer().span_tokenize(sentence)]
    tokens = nltk.pos_tag(words)
    chunk_parser = nltk.RegexpParser(GRAMMAR)
    tree = chunk_parser.parse(tokens)
    return tree, tokens, spans


def merge_tree_with_spans(tree, spans):
    tree_with_spans = []
    idx = 0
    for node in tree:
        idx, node_with_spans = merge_node_with_spans(node, spans, idx)
        tree_with_spans.append(node_with_spans)
    return tree_with_spans


def merge_node_with_spans(node, spans, idx):
    if isinstance(node, nltk.Tree):
        leaveSize = len(node.leaves())
        begin = spans[idx][0]
        idx += leaveSize
        end = spans[idx - 1][1]
        return idx, [node, [begin, end]]
    else:
        span = spans[idx]
        idx += 1
        return idx, [node, span]


def run_task(f, *args, **kwargs):
    t = Thread(target=f, args=args, kwargs=kwargs)
    t.start()


def add_overlay(sentence):
    tree, tokens, spans = split_sentence(sentence)
    tree_with_spans = merge_tree_with_spans(tree, spans)
    token_with_spans = merge_token_with_spans(tokens, spans)
    add_overlay_for_tree(tree_with_spans)


def merge_token_with_spans(tokens, spans):
    token_with_spans = []
    for i in range(len(tokens)):
        token_with_spans.append([tokens[i], spans[i]])
    return token_with_spans


def add_overlay_for_tree(tree_with_spans):
    print (tree_with_spans)
    for node in tree_with_spans:
        if isinstance(node[0], nltk.Tree):
            print(node[1])
            client.call_sync("nltk-overlay-current-line-from", node[1])


def classify_word_in_sentence(sentence, column):
    tree, tokens, spans = split_sentence(sentence)
    tree_with_spans = merge_tree_with_spans(tree, spans)
    token_with_spans = merge_token_with_spans(tokens, spans)
    classify_word_in_tokens(token_with_spans, column)


def classify_word_in_tokens(token_with_spans, column):
    for token in token_with_spans:
        if column >= token[1][0] and column < token[1][1]:
            cls = token[0][1]
            client.call_sync("nltk-echo", ["{} : {}".format(token[0],NLTK_CLASS[cls])])


@server.register_function
def parse_sentence(*a):
    print(*a)
    run_task(add_overlay, *a)
    return a


@server.register_function
def classify_word(*a):
    print(*a)
    run_task(classify_word_in_sentence, *a)
    return a


@server.register_function
def set_epcs_port(*a):
    print(*a)
    global client
    client = EPCClient(("localhost", *a))
    return a


def main():
    server.print_port()
    server.serve_forever()


if __name__ == "__main__":
    main()
