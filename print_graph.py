from pygraphviz import AGraph as G
from typing import List
from sys import argv
from os import fsencode, fsdecode, listdir

DOT_DIR = "dots"
DOT_EXT = "dot"
SVG_EXT = "svg"
IMG_DIR = "graphs"

def draw_graph(
        filename: str,
        graph_ext: str = DOT_EXT,
        graph_dir: str = DOT_DIR,
        img_dir: str = IMG_DIR,
        img_ext: str = SVG_EXT
        ) -> None:
    """
    Reads graph metadata GRAPH_DIR/FILENAME.GRAPH_EXT
    Draws a graph image IMG_DIR/FILENAME.IMG_EXT
    Default: dots/<filename>.dot to imgs/<filename>.svg
    """
    filepath = "{}/{}".format(graph_dir, filename)
    graph = G(filepath)
    graph.layout()
    img_filename = filename.replace(graph_ext, img_ext)
    graph.draw("{}/{}".format(img_dir, img_filename))

def loop_dir(dir_name: str, graph_ext: str) -> None:
    """
    Loops through a directory
    Draws graphs from all the graph metadata files
    """
    directory = fsencode(dir_name)
    for file in listdir(directory):
        filename = fsdecode(file)
        if filename.endswith(graph_ext):
            draw_graph(filename)

def run():
    loop_dir(DOT_DIR, DOT_EXT)

run()
