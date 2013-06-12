#!/usr/bin/env bash

# requires PIL (sudo apt-get install python-imaging, brew install pil)

usage='USAGE: sync-photos.sh SOURCE_DIR SINK_DIR [SIMULATE 1|0]'
test $# -lt 2 && echo -e $usage && exit 1

src=$1
snk=$2
simulate=`test $# -gt 2 && test $3 = 1 && echo True || echo False`


tmpdir=`mktemp -d`
existing=$tmpdir/existing
to_import=$tmpdir/to_import
mkdir -p $snk

echo 'INFO - Started at '`date`
test $simulate = True && echo 'INFO - Simulating!'

## find photos in sink directory (only basenames)
find $snk -iname "*.jpg" -or -iname "*.png" -type f | xargs -I {} python -c "
import os

path = '"{}"'
print os.path.basename(path)
" | sort | uniq > $existing


## find photos to import
find $src -iname "*.jpg" -or -iname "*.png" -type f > $to_import

## generate filenames for photos to import
python -c "
from PIL import Image
from PIL.ExifTags import TAGS
import re
import os
import sys

## simple string normalization
def str_norm(s):
  return re.sub(r'[^A-Za-z0-9_]', '',
                re.sub(r'\s+', '_', s)).lower()

snk = '"$snk"'

## load photos that exist
with open('"$existing"') as f:
  existing = set(map(lambda x: x.strip(), f.readlines()))

## load photos to import
with open('"$to_import"') as f:
  to_import = map(lambda x: x.strip(), f.readlines())

## loop over photos to import
for path in to_import:
  ## read exif data
  ret = dict()
  i = Image.open(path)
  info = i._getexif()
  for tag, value in info.items():
    decoded = TAGS.get(tag, tag)
    ret[decoded] = value

  make = str_norm(ret['Make'])
  model = str_norm(ret['Model'])
  timestamp = str_norm(ret['DateTimeOriginal'])
  id = re.search(r'(.*)_IMG_([0-9]+)\..*',path).group(2)

  ## get extension
  ext = str_norm(os.path.splitext(path)[1])

  ## create new path
  new_path = '%(timestamp)s-%(make)s-%(model)s-%(id)s.%(ext)s' % {'make': make, 'model': model, 'timestamp': timestamp, 'ext': ext, 'id': id}

  ## print the source and sink paths if need to be copied
  if new_path in existing:
    sys.stderr.write('WARNING - %s exists (skipping)\n' % new_path)
  else:
    new_path = os.path.join(snk, new_path)
    print '%s\v%s' % (path, new_path)
" | \
xargs -P 5 -I {} python -c "
import sys
import shutil

simulate = "$simulate"

pair = '"{}"'.strip()
path, new_path = pair.split('\v')

sys.stderr.write('INFO - copying %s to %s %s\n' % (path, new_path, '(simulating)' if simulate else ''))
## copy file from source to sink if not simulating
if not simulate:
  try:
    shutil.copy(path, new_path)
  except e:
    sys.stderr.write('ERROR - error while copying %s - %s\n' % (path, e))
"

echo 'INFO - Ended at '`date`












# import('utils'); load.data('digikam_albums_table.csv',header=T,sep=',',quote='"') -> x; subset(x,select=c(relativePath,caption)) -> y; y$relativePath <- gsub('_','',sapply(str_split(as.character(y$relativePath), '/'), function(e) tail(e,1))); y$caption <- gsub(';(_| );',';',gsub('_+','_',gsub('\\s*:\\s*',' - ',gsub('\\s+',' ',gsub('(\n|;\\s*|；\\s*)',' ; ', str_trim(as.character(y$caption))))))); y <- subset(y, !str_detect(caption,'^\\s*$')); write.table(named(y, c('date','caption')),file='~/Pictures/photos/digikam_albums_table.tsv',sep='\t',col.names=T,row.names=F,quote=F); file.show('~/Pictures/photos/digikam_albums_table.tsv')

# cat /home/elliot/Pictures/photos/digikam_albums_table.tsv |tail -n+2 | python -c "
# import sys,os,re
# for ln in sys.stdin:
#   p = re.sub(\"'\",\"\\\\'\",' : '.join(ln.strip().split('\t')))
#   p = re.sub(\" \",\"\\ \", p)
#   p = re.sub(\";\",\"\\;\", p)
#   print p" | xargs touch


