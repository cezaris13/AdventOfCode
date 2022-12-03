#include <bits/stdc++.h>

using namespace std;
vector<int> returnListOfInts(string inputNumbers){
  vector<int> vect;
  stringstream ss(inputNumbers);

  for (int i; ss >> i;) {
    vect.push_back(i);
    if (ss.peek() == ',')
      ss.ignore();
  }
  return vect;
}

int main(){
    freopen("input.txt","r",stdin);
    string input;
    cin>>input;
    vector<int> elements=returnListOfInts(input);
    // Part 2
    unsigned long long fish_count[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    int days = 256;
    for(auto el : elements)
        fish_count[el]++;
    for(int i=0;i<days;i++){
        unsigned long long temp = fish_count[0];
        fish_count[0] = fish_count[1];
        fish_count[1] = fish_count[2];
        fish_count[2] = fish_count[3];
        fish_count[3] = fish_count[4];
        fish_count[4] = fish_count[5];
        fish_count[5] = fish_count[6];
        fish_count[6] = fish_count[7] + temp;
        fish_count[7] = fish_count[8];
        fish_count[8] = temp;
    }

    auto count = 0ULL;
    for (auto i : fish_count)
        count += i;

    cout<<count<<endl;
    // part 1
    // for(int i=0;i<80;i++)
    // for(int j=0;j<elements.size();j++){
    //     elements[j]--;
    //     if(elements[j]==-1){
    //         elements[j]=6;
    //         elEMENT.push_back(9);
    //     }
    // }
    // }
    // cout<<elements.size()<<endl;
}
