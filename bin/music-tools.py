#!/usr/bin/env python

import eyed3
import os
import sys
import datetime as dt
import shutil

########################################################
#### OPs
########################################################
VALID_OPS = ['tagsfromfilename', 'renameusingtags', "help", "h", "copytolibrary"]

def tags_from_filename(input_dir, execute):
  for full_path in list_dir(input_dir, include = lambda x: not os.path.isdir(x) and os.path.basename(x).endswith(".mp3")):
    try:
      metadata = eyed3.load(full_path)
    except Exception, err:
      log("failed to load tags for %s" % full_path, "error: ", err)
      continue
    base_name = os.path.basename(full_path.strip())

    parts = map(lambda x: x.replace('_',' ').strip(), base_name.replace('.mp3','').replace(" ", "_").split('_-_'))

    artist = unicode(parts[0])
    if len(parts) >= 3:
      album = unicode(parts[1])
      title = unicode(parts[2])
    else:
      album = unicode(' ')
      title = unicode(parts[1])

    log(full_path, '| ARTIST:', metadata.tag.artist, '->', artist, '| ALBUM:', metadata.tag.album, '->', album, '| TITLE:', metadata.tag.title, '->', title)

    if execute:
      metadata.tag.artist = artist
      metadata.tag.album = album
      metadata.tag.title = title
      metadata.tag.save()
      log(full_path, 'saved!')

def rename_using_tags(input_dir, execute):
  for full_path in list_dir(input_dir, include = lambda x: not os.path.isdir(x) and os.path.basename(x).endswith(".mp3")):
    try:
      metadata = eyed3.load(full_path)
    except Exception, err:
      log("failed to load tags for %s" % full_path, "error: ", err)
      continue

    dir_name = os.path.dirname(full_path)

    ext = os.path.splitext(full_path)[1]
    name = ' - '.join(
      [metadata.tag.artist] +
      ([metadata.tag.album] if metadata.tag.album and metadata.tag.album.strip() else []) +
      [metadata.tag.title]
      )

    dest_base_name = name + ext

    full_dest_path = os.path.join(dir_name, dest_base_name).replace(' ', '_')

    if full_path != full_dest_path:
      log(full_path, '->', full_dest_path)
      if execute:
        os.rename(full_path, full_dest_path)
        log(full_path, 'renamed!')

def copy_to_library(input_dir, execute):
  lib_dir = os.path.join(os.getenv("HOME"), "Music")

  for full_path in list_dir(input_dir, include = lambda x: not os.path.isdir(x) and os.path.basename(x).endswith(".mp3")):
    try:
      metadata = eyed3.load(full_path)
    except Exception, err:
      log("failed to load tags for %s" % full_path, "error: ", err)
      continue

    artist_dir = os.path.join(lib_dir, metadata.tag.artist.replace(" ", "_"))
    dest_path = os.path.join(artist_dir, os.path.basename(full_path).replace(" ", "_"))

    if full_path != dest_path:
      if not os.path.exists(artist_dir):
        log("create artist dir:", artist_dir)
        if execute:
          os.mkdir(artist_dir)


      log(full_path, '->', dest_path)
      if execute:
        shutil.copyfile(full_path, dest_path)
        log(full_path, 'copied!')

########################################################
#### utils
########################################################
def list_dir(dir, include = lambda x: True, exclude = lambda x: os.path.basename(x).startswith(".")):
  q = []
  def children(dir):
    res = []
    if os.path.isdir(dir):
      res = map(lambda d: os.path.join(dir, d), sorted(os.listdir(dir)))
    return res
  def enqueue_contents(dir):
    q.extend(children(dir))
  def dequeue():
    return q.pop(0)

  enqueue_contents(dir)

  while q:
    nxt = dequeue()
    keep = include(nxt) and not exclude(nxt)
    enqueue_contents(nxt)
    if keep:
      yield nxt


def log(*msgs):
  now = dt.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
  sys.stderr.write(now)
  sys.stderr.write(' - ')
  sys.stderr.write(' '.join(map(str, msgs)))
  sys.stderr.write('\n')

def usage_string():
  return ("""
USAGE: music-tools.py OP INPUT_DIR
    OP operation (%(ops)s)""" % {
  'ops': ','.join(sorted(VALID_OPS))
  })

def print_usage(exit = None):
  sys.stderr.write(usage_string())
  sys.stderr.write('\n')
  if exit != None: sys.exit(exit)


########################################################
#### main
########################################################

if __name__ == '__main__':
  try:
    op = sys.argv[1].lower()
    if not op in VALID_OPS: raise Exception('unknown operation %s' % op)

    if op in ["help", "h"]:
      print_usage(0)

    input_dir = sys.argv[2]
    execute = "execute" in sys.argv[3:]
  except Exception, err:
    log(err)
    print_usage(1)

  log("op:", op)
  log("input dir:", input_dir)
  log("execute: ", execute)


  if op == "tagsfromfilename":
    tags_from_filename(input_dir = input_dir, execute = execute)
  elif op == "renameusingtags":
    rename_using_tags(input_dir = input_dir, execute = execute)
  elif op == "copytolibrary":
    copy_to_library(input_dir = input_dir, execute = execute)
