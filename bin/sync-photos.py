from PIL import Image
from PIL.ExifTags import TAGS
import re
import os
import sys
import md5

def listdir(path):
  return map(lambda x: os.path.join(path,x), os.listdir(path))

def rlistdir(path):
  out = listdir(path)
  remaining = filter(os.path.isdir, out)
  while remaining:
    p = remaining[0]
    ps = listdir(p)
    out += ps
    remaining = remaining[1:] + filter(os.path.isdir, ps)
  return out

def rlistimages(path, ext = ['.jpg','.png']):
  return filter(lambda x: os.path.splitext(x)[1].lower() in ext, rlistdir(path))

def str_norm(s):
  return re.sub(r'[^A-Za-z0-9_]', '',
                re.sub(r'\s+', '_', s)).lower()


if __name__ == '__main__':
  if len(sys.argv) < 3:
    print "USAGE: python sync-photos.py SOURCE_DIR SINK_DIR (RUN 0|1)]"

  src = sys.argv[1]
  snk = sys.argv[2]
  simulate = not (len(sys.argv) > 3 and sys.argv[3] == '1')

  to_import = rlistimages(src)
  existing = dict(map(lambda x: (os.path.basename(x), x), rlistimages(snk)))

  imported = 0
  failures = 0
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

      # id = re.search(r'(.*)_-_(.*)..*?',path).group(2)
      # id = md5.new(path).hexdigest()
      id = str_norm(os.path.basename(os.path.splitext(path)[0]))

      ## get extension
      ext = str_norm(os.path.splitext(path)[1])

      ## create new path
      new_path = '%(timestamp)s-%(make)s-%(model)s-%(id)s.%(ext)s' % {'make': make, 'model': model, 'timestamp': timestamp, 'ext': ext, 'id': id}

      ## print the source and sink paths if need to be copied
      if new_path in existing:
        print 'skip\t%s\t%s' % (path, existing[new_path])
      else:
        new_path = os.path.join(snk, new_path)
        print '%s\t%s\t%s' % ('to_copy' if simulate else 'copy', path, new_path)
        imported += 1
        if not simulate:
          try:
            shutil.copy(path, new_path)
          except e:
            sys.stderr.write('ERROR - error while copying %s - %s\n' % (path, e))
            failures += 1
    except Exception,e:
      sys.stderr.write('ERROR - problem processing %s: %s\n' % (path, e.message))
      failures += 1

  if failures:
    print "ERROR: there were", failures, "failure(s)"
    sys.exit(1)
  else:
    print "Done! Examined:", len(to_import), "Imported:", imported
    sys.exit(0)
