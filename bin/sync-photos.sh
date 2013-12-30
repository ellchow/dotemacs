

# requires PIL (sudo apt-get install python-imaging, brew install pil)

usage='USAGE: sync-photos.sh SOURCE_DIR SINK_DIR [SIMULATE 1|0]'
test $# -lt 2 && echo -e $usage && exit 1

src=$1
snk=$2
simulate=`test $# -gt 2 && test $3 = 1 && echo True || echo False`


tmpdir=`mktemp -d /tmp/XXXX`
existing=$tmpdir/existing
to_import=$tmpdir/to_import
mkdir -p $snk

echo 'INFO - tmp directory' $tmpdir >&2
echo 'INFO - Started at '`date`
test $simulate = True && echo 'INFO - Simulating!' >&2

## find photos in sink directory
echo 'INFO - finding exisitng photos' >&2
find $snk -iname "*.jpg" -or -iname "*.png" -type f | sort | uniq > $existing

## find photos to import
echo 'INFO - finding photos to import' >&2
find $src -iname "*.jpg" -or -iname "*.png" -type f > $to_import

## generate filenames for photos to import
python -c "
from PIL import Image
from PIL.ExifTags import TAGS
import re
import os
import sys
import md5

## simple string normalization
def str_norm(s):
  return re.sub(r'[^A-Za-z0-9_]', '',
                re.sub(r'\s+', '_', s)).lower()

snk = '"$snk"'

## load photos that exist
with open('"$existing"') as f:
  existing = set(map(lambda x: os.path.basename(x.strip()), f.readlines()))

## load photos to import
with open('"$to_import"') as f:
  to_import = map(lambda x: x.strip(), f.readlines())

## loop over photos to import
for path in to_import:
  try:
    ## read exif data
    ret = dict()
    i = Image.open(path)
    info = i._getexif()
    for tag, value in info.items():
      decoded = TAGS.get(tag, tag)
      ret[decoded] = value

    make = str_norm(ret['Make']) if 'Make' in ret else 'NoMake'
    model = str_norm(ret['Model']) if 'Model' in ret else 'NoModel'
    timestamp = str_norm(ret['DateTimeOriginal']) if 'DateTimeOriginal' in ret else 'NoDateTimeOriginal'

    #id = re.search(r'(.*)_-_(.*)..*?',path).group(2)
    # id = md5.new(path).hexdigest()
    id = str_norm(os.path.splitext(path)[0])


    ## get extension
    ext = str_norm(os.path.splitext(path)[1])

    ## create new path
    new_path = '%(timestamp)s-%(make)s-%(model)s-%(id)s.%(ext)s' % {'make': make, 'model': model, 'timestamp': timestamp, 'ext': ext, 'id': id}

    ## print the source and sink paths if need to be copied
    if new_path in existing:
      sys.stderr.write('WARNING - %s exists (skipping)\n' % new_path)
    else:
      new_path = os.path.join(snk, new_path)
      print '%s\t%s' % (path, new_path)
  except Exception,e:
    sys.stderr.write('ERROR - problem processing %s: %s\n' % (path, e.message))

" | python -c "

import sys
import shutil

simulate = "$simulate"

for ln in sys.stdin:
  pair = ln.strip()
  path, new_path = pair.split('\t')

  sys.stderr.write('INFO - copying %s to %s %s\n' % (path, new_path, '(simulating)' if simulate else ''))
  ## copy file from source to sink if not simulating
  if not simulate:
    try:
      shutil.copy(path, new_path)
    except e:
      sys.stderr.write('ERROR - error while copying %s - %s\n' % (path, e))""

"
echo 'INFO - Ended at '`date`












# import('utils'); load.data('digikam_albums_table.csv',header=T,sep=',',quote='"') -> x; subset(x,select=c(relativePath,caption)) -> y; y$relativePath <- gsub('_','',sapply(str_split(as.character(y$relativePath), '/'), function(e) tail(e,1))); y$caption <- gsub(';(_| );',';',gsub('_+','_',gsub('\\s*:\\s*',' - ',gsub('\\s+',' ',gsub('(\n|;\\s*|；\\s*)',' ; ', str_trim(as.character(y$caption))))))); y <- subset(y, !str_detect(caption,'^\\s*$')); write.table(named(y, c('date','caption')),file='~/Pictures/photos/digikam_albums_table.tsv',sep='\t',col.names=T,row.names=F,quote=F); file.show('~/Pictures/photos/digikam_albums_table.tsv')

# cat /Volumes/WD\ Passport/SYNCed/Pictures/photos/digikam_albums_table.tsv |tail -n+2 | python -c "
# import sys,os,re
# for ln in sys.stdin:
#   p = re.sub(\"'\",\"\\\\'\",' : '.join(ln.strip().split('\t')))
#   p = re.sub(\" \",\"\\ \", p)
#   p = re.sub(\"/\",\"\\ \", p)
#   p = re.sub(\";\",\"\\;\", p)
#   print p + '.info'" | xargs touch
