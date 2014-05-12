#include <iostream>
#include <istream>
#include <ostream>

#include <cmath>
#include <vector>
#include <map>
#include <list>
#include <utility>

using namespace std;

typedef long mint;

vector<mint> ns;
vector<list<mint> > es;
map<pair<mint, mint>, mint> m;

mint calc(mint a, mint b)
{
	if (m.find(make_pair(a, b)) == m.end())
	{
		mint s = ns[b];

		for (list<mint>::iterator iter = es[b].begin();
				iter != es[b].end(); ++iter)
		{
			mint c = *iter;

			if (a == c)
			{
				continue;
			}

			s += calc(b, c);
		}

		m[make_pair(a, b)] = s;
	}

	return m[make_pair(a, b)];
}

int main()
{
	mint n;

	cin >> n;

	ns = vector<mint>(n);
	es = vector<list<mint> >(n);

	for (mint ix = 0; ix != n; ++ix)
	{
		mint x;

		cin >> x;

		ns[ix] = x;
	}

	for (mint ix = 0; ix != n - 1; ++ix)
	{
		mint a, b;

		cin >> a >> b;

		--a;
		--b;

		es[a].push_front(b);
		es[b].push_front(a);
	}

	for (mint a = 0; a != n; ++a)
	{
		for (list<mint>::iterator iter = es[a].begin();
				iter != es[a].end(); ++iter)
		{
			mint b = *iter;

			calc(a, b);
		}
	}

	mint best = -1;

	for (map<pair<mint, mint>, mint>::iterator iter = m.begin();
			iter != m.end(); ++iter)
	{
		mint d = abs(iter->second - m[make_pair(iter->first.second, iter->first.first)]);

		if (best == -1 || d < best)
		{
			best = d;
		}
	}

	cout << best << endl;

	return 0;
}

