
# print header to stdout, 1 column per line
header(){
    if [ ${#1} = 0 ]; then
        sep="\t"
    else
        sep=$1
    fi
    head -n1 | gawk '
BEGIN{
RS="'$sep'"
OFS="\n"
}
{ print(NR "\t" $0) }
'
}

# apply identity transformation on 1st line (header), then apply transformation on body (rest)
onbody() {
    IFS= read -r header
    printf '%s\n' "$header"
    if [ ${#1} = 0 ]; then
        cat
    else
        "$@"
    fi
}

# store header mapping (1st line) in IDX array
awkh(){
    awk "
NR==1{
  for(i=1;i<=NF;i+=1){
    IDX[\$i] = i
  }
}
""$@"
}
