#include <iostream>
#include <istream>
#include <ostream>

#include <vector>
#include <list>
#include <unordered_set>

using namespace std;

typedef int mint;

vector<list<mint> > children;
vector<mint> cs;
vector<mint> cns;

unordered_set<mint> bt(mint n, unordered_set<mint> visited)
{
    visited.insert(n);

    unordered_set<mint> r;

    r.insert(cs[n]);

    for (list<mint>::iterator iter = children[n].begin();
            iter != children[n].end(); ++iter)
    {
        if (visited.find(*iter) == visited.end())
        {
            unordered_set<mint> q = bt(*iter, visited);

            r.insert(q.begin(), q.end());
        }
    }

    cns[n] = r.size();

    return r;
}

int main()
{
    mint n, m, rt;

    cin >> n >> m >> rt;

    --rt;

    mint h, t;

    children = vector<list<mint> >(n);

    for (mint ix = 0; ix != n - 1; ++ix)
    {
        cin >> h >> t;

        --h;
        --t;

        children[h].push_back(t);
        children[t].push_back(h);
    }

    cs = vector<mint>(n);

    mint c;

    for (mint ix = 0; ix != n; ++ix)
    {
        cin >> c;

        cs[ix] = c;
    }

    cns = vector<mint>(n);

    bt(rt, unordered_set<mint>());

    mint q;

    for (mint ix = 0; ix != m; ++ix)
    {
        cin >> q;

        --q;

        cout << cns[q] << endl;
    }

    return 0;
}
