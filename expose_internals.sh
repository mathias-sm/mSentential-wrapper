#!/bin/bash

# Check args and provide help
if [ "$#" -ne 3 ]; then
  echo "Use: ./run_once_verbose.sh sigma gamma (formula)"
  exit 0
fi

original_file="mSentential-v1.1.lisp"
utf8_file="mSentential-v1.1_utf8.lisp"
patched_file="mSentential-v1.1_utf8_expose_internals.lisp"

# Defines the patch to apply to mSentential to expose the internals
read -r -d '' patch <<EOF
--- mSentential-v1.1_utf8.lisp	2019-12-16 16:48:07.446675883 +0100
+++ mSentential-v1.1_utf8_expose_internals.lisp	2019-12-16 16:48:47.643342739 +0100
@@ -518 +518 @@
-              (trc "System 1" (format nil "The premises yield the models")) 
+              (trc "System 1" (format nil "The premises yield the models: ~A" (build-models premises))) 
@@ -2678,0 +2679,5 @@
+(initialize-tracer :verbose t)
+(setf *sigma* ###-sigma-placeholder-###)
+(setf *gamma* ###-gamma-placeholder-###)
+(inference '(###-formula-placeholder-###))
+(quit)
EOF

# Builds the test lisp file from the original file by converting it to UTF8,
# patching it and cleaning it up
if [ ! -f $patched_file ]; then
  if [ ! -f $utf8_file ]; then
    echo -e "[\033[93mWARNING\033[0m] mSentential lisp file absent: downloading it"
    curl -s "http://modeltheory.org/programs/$original_file" -o $original_file
    iconv -f ISO-8859-15 -t UTF-8 $original_file > $utf8_file
    rm $original_file
  fi
  echo -e "[\033[93mWARNING\033[0m] Patched mSentential lisp file absent: patching the original file"
  cp $utf8_file $patched_file
  patch -s $patched_file <<< "$patch"
fi

# Replace the placeholders in the lisp file by the script arguments
sed -e "s/###-sigma-placeholder-###/$1/" $patched_file |
  sed -e "s/###-gamma-placeholder-###/$2/" |
  sed -e "s/###-formula-placeholder-###/$3/" > tmp.lisp

# Run the test and clean up
ecl -q --load split-sequence.lisp --load tmp.lisp
rm tmp.lisp
