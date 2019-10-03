i=0
for foo in $(ls -t Screenshot*); do
    bar=$(printf "%02d" $i)
    cp -i "$foo" progress-$bar.png
    i=$((i+1))
done
