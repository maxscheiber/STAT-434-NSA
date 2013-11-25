from datetime import datetime

# consolidated trade data from WRDS
READ_NAME = 'goog_consolidated.csv'

f = open(READ_NAME, 'r')

# Because the trade data are ordered, we can do this one bucket at a time
cur_price = 0
cur_volume = 0
cur_count = 0
cur_bucket = datetime(2013, 1, 2, 9) # We start with the 9am bucket on January 2nd
cur_date_str = "20130102" # For saving purposes only
cur_hour = 9 # Ibid, your honor

buckets = []

raw_trades = f.read().split() # split on whitespace, so here, newlines
del raw_trades[0] # George Bush doesn't care about column names

# SYMBOL,DATE,TIME,PRICE,VOLUME
for trade in raw_trades:
	# This is a comma-deliminited line
	trade_data = trade.split(",")

	# These are the five values of the file from WRDS
	symbol = trade_data[0]
	date = trade_data[1]
	time = trade_data[2]
	price = trade_data[3]
	size = trade_data[4]

	# These are the date and time values parsed from the input
	year = int(date[0:4])
	month = int(date[4:6])
	day = int(date[6:8])
	hour = int(time.split(':')[0])

	tmp_bucket = datetime(year, month, day, hour)

	# We're at a new bucket!
	if (cur_bucket != tmp_bucket):
		# Remember, price is in cents!
		bucket = dict(symbol = symbol, volume = cur_volume, price = cur_price / cur_count, date = cur_date_str, time = cur_hour)
		buckets.append(bucket)
		cur_bucket = tmp_bucket
		cur_date_str = date
		cur_hour = hour

		# Reset those accumulators!
		cur_price = 0
		cur_volume = 0
		cur_count = 0

	# Process the new data
	cur_price += int(100 * float(price))
	cur_count += 1
	cur_volume += int(size)

# 4pm buckets only contain trades at 4:00:00pm, so let's drop them.
adj_buckets = filter(lambda t: t['time'] != 16, buckets)

# Now, to re-convert into a similar, usable CSV
g = open('goog_hourly.csv', 'w')
g.write("SYMBOL,DATE,HOUR,PRICE,SIZE\n")
for bucket in adj_buckets:
	cents_price_str = str(bucket['price'])
	dollars_price = cents_price_str[0:len(cents_price_str)-2] + "." + cents_price_str[len(cents_price_str) - 2:]
	g.write(",".join([bucket['symbol'], bucket['date'], str(bucket['time']), dollars_price, str(bucket['volume'])]))
	g.write("\n")