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
int findMedian(vector<int> ints){
    sort(ints.begin(),ints.end());
    if(ints.size()%2!=0){
        return ints[ints.size()/2];
    }
    else{
        return (ints[ints.size()/2]+ints[ints.size()/2-1])/2;
    }
}
int findAverage(vector<int> ints){
    double sum = 0;
    for(int i=0;i<ints.size();i++){
        sum+=ints[i];
    }
    double avg=sum/ints.size();
    return ceil(avg);
}
int main()
{
  freopen("input.txt","r",stdin);
  string input;
  cin>> input;
  vector<int> elements = returnListOfInts(input);
  int median=findMedian(elements);
  int average=findAverage(elements);
  int totalCost = 0;
  //part1
  for(int i=0;i<elements.size();i++){
      totalCost+=abs((elements[i]-(median)));
  }
  cout<<totalCost<<endl;
  totalCost=0;
  //part2
  cout<<average<<endl;
  for(int i=0;i<elements.size();i++){
      totalCost+=abs((elements[i]-(average))*(elements[i]-average+1)/2);
  }
  cout<<totalCost<<endl;
}
