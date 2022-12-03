#include <vector>
#include <algorithm>
#include <string>
#include <fstream>
#include <iostream>
#include <bits/stdc++.h>

using namespace std;
int getCost(int spent){
    int sum = 0;
    for (int i = 1; i <= spent; ++i) {
       sum += i;
    }
    return sum;
}
int main() {
    vector<int> v;
    int N;
    ifstream fin("input.txt");
    ofstream fout("day7.out");
    int m = 0;
    cout << getCost(abs(16-5)) << endl;
    while(fin>>N){
        char x;
        fin >> x;
        v.push_back(N);
        m = max(m,N);
    }
    int mincost = INT_MAX;
    for (int i = 0; i <= m; ++i) {
        int cost = 0;
        for (const auto &item : v){
           cost +=getCost(abs(item-i));
        }
        mincost = min(cost, mincost);
    }
    cout << mincost << endl;
    return 0;
}
