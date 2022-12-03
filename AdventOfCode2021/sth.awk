#!/usr/bin/awk -f

BEGIN { DAYS = 80; RS = ","; FS = "" }
      { a[$1]++ }
END   {
	for (i = 0; i < DAYS; i++)
		a[(i + 7) % 9] += a[i % 9]
	for (i in a)
		n += a[i]
	print n
}
