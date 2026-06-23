import csv, collections, statistics

with open('car_sales_past90days_FINAL.csv', encoding='utf-8') as f:
    reader = csv.DictReader(f)
    rows = list(reader)

print(f'Total rows: {len(rows)}')
print(f'Columns: {list(rows[0].keys())}')

prices = [float(r["price"]) for r in rows if r["price"] not in ("", "NA", "None")]
miles  = [float(r["miles"]) for r in rows if r["miles"] not in ("", "NA", "None")]
years  = [int(r["year"]) for r in rows if r["year"] not in ("", "NA", "None") and r["year"].strip().isdigit()]

print(f'\nPrice: min={min(prices):.0f}  max={max(prices):.0f}  median={statistics.median(prices):.0f}  mean={statistics.mean(prices):.0f}')
print(f'Miles: min={min(miles):.0f}  max={max(miles):.0f}  median={statistics.median(miles):.0f}  mean={statistics.mean(miles):.0f}')
print(f'Year:  min={min(years)}  max={max(years)}  mean={statistics.mean(years):.0f}')

# Missing values
print('\n=== MISSING VALUES ===')
for col in rows[0].keys():
    missing = sum(1 for r in rows if r[col] in ('', 'NA', 'None', 'nan'))
    if missing > 0:
        print(f'  [{col}]: {missing} missing ({100*missing/len(rows):.1f}%)')

# Top makes
makes = collections.Counter(r['make'] for r in rows if r['make'] not in ('', 'NA'))
print('\n=== TOP 10 MAKES ===')
for make, count in makes.most_common(10):
    print(f'  {make}: {count}')

# Body types
bodies = collections.Counter(r['body_type'] for r in rows if r['body_type'] not in ('', 'NA'))
print('\n=== BODY TYPES ===')
for bt, count in bodies.most_common():
    print(f'  {bt}: {count}')

# Tier counts
tiers = collections.Counter(r['affordability_tier'] for r in rows)
print('\n=== CURRENT TIER COUNTS ===')
for t, c in tiers.most_common():
    print(f'  {t}: {c}')

# Seller types
sellers = collections.Counter(r['seller_type'] for r in rows)
print('\n=== SELLER TYPES ===')
print(dict(sellers))

# Price extremes
print('\n=== TOP 10 HIGHEST PRICED ===')
big = sorted([r for r in rows if r['price'] not in ('','NA')], key=lambda x: -float(x['price']))
for r in big[:10]:
    print(f'  {r["year"]} {r["make"]}  price={float(r["price"]):.0f}  miles={r["miles"]}  body={r["body_type"]}')

# Classic cars
old = [r for r in rows if r['year'].strip().isdigit() and int(r['year']) <= 1985]
print(f'\n=== YEAR <= 1985 (count={len(old)}) ===')
for r in sorted(old, key=lambda x: -float(x['price']) if x['price'] not in ('','NA') else 0)[:10]:
    print(f'  {r["year"]} {r["make"]}  price={r["price"]}  miles={r["miles"]}')

# Very cheap
cheap = [r for r in rows if r['price'] not in ('','NA') and float(r['price']) < 3000]
print(f'\n=== PRICE < 3000 (count={len(cheap)}) ===')
for r in cheap[:8]:
    print(f'  {r["year"]} {r["make"]}  price={r["price"]}  miles={r["miles"]}  seller={r["seller_type"]}')

# Price distribution buckets
buckets = {'<10k':0, '10-20k':0, '20-30k':0, '30-45k':0, '45-100k':0, '100k+':0}
for r in rows:
    if r['price'] in ('','NA'): continue
    p = float(r['price'])
    if p < 10000: buckets['<10k'] += 1
    elif p < 20000: buckets['10-20k'] += 1
    elif p < 30000: buckets['20-30k'] += 1
    elif p < 45000: buckets['30-45k'] += 1
    elif p < 100000: buckets['45-100k'] += 1
    else: buckets['100k+'] += 1
print('\n=== PRICE DISTRIBUTION ===')
for k, v in buckets.items():
    print(f'  {k}: {v}')

# DOM distribution
doms = [float(r['dom_active']) for r in rows if r['dom_active'] not in ('','NA')]
print(f'\n=== DOM ACTIVE: min={min(doms):.0f}  max={max(doms):.0f}  median={statistics.median(doms):.0f}  mean={statistics.mean(doms):.0f} ===')
