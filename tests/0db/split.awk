BEGIN { RS="@"; count=0 }
{
	# trim leading and trailing whitespace
	gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", $0)
	if (length($0) > 0) {			# line must be non-empty
		filename = file_prefix count	# generate current filename
		print > filename		# write current portion to file
		close(filename)
		count++
	}
}
