#include <bits/stdc++.h>

using namespace std;

// int main(){
//     int minNumber=0,maxNumber=0;
//     freopen("input.txt","r",stdin);
//     int n=0;
//     map<int,int> checkBits;
//     while(!cin.eof()){
//         string input;
//         cin>>input;
//         for(int i =0;i<input.size();i++){
//             checkBits[i] +=(input[i]=='0'?-1:1);
//         }
//     }
//     int i=0;
//     for(auto iter = checkBits.rbegin(); iter != checkBits.rend(); ++iter){
//         maxNumber+=pow(2,i)*(iter->second>0?1:0);
//         minNumber+=pow(2,i)*(iter->second>0?0:1);
//         i++;
//     }
//     cout<<maxNumber<<" "<<minNumber<<endl;
//     cout<<minNumber*maxNumber<<endl;
// }
int getBiggestNumber(vector<string> listOfNumbers,int position){
    int count = 0;
    for(int i=0;i<listOfNumbers.size();i++){
        count +=(listOfNumbers[i][position]=='0'?-1:1);
    }
    return count>=0?1:0;
}

int main(){
    int oxygenGenerator=0,co2Scrubber=0;
    freopen("input.txt","r",stdin);
    int n=0;
    vector<string> listOfMaxiNumbers;
    vector<string> listOfMiniNumbers;
    while(!cin.eof()){
        string input;
        cin>>input;
        if(input.size()>0){
            listOfMaxiNumbers.push_back(input);
            listOfMiniNumbers.push_back(input);
        }
    }
    int i=0;
    while(listOfMaxiNumbers.size()>1){
        int positionBit = getBiggestNumber(listOfMaxiNumbers,i);
        for(int j=0;j<listOfMaxiNumbers.size();j++){
            if(listOfMaxiNumbers[j][i]-48 != positionBit && listOfMaxiNumbers.size()>1){
                listOfMaxiNumbers.erase(listOfMaxiNumbers.begin()+j,listOfMaxiNumbers.begin()+j+1);
                j--;
            }
        }
        i++;
    }
    i=0;
    while(listOfMiniNumbers.size()>1){
        int positionBit = getBiggestNumber(listOfMiniNumbers,i);
        for(int j=0;j<listOfMiniNumbers.size();j++){
            if(listOfMiniNumbers[j][i]-48 == positionBit && listOfMiniNumbers.size()>1){
                listOfMiniNumbers.erase(listOfMiniNumbers.begin()+j,listOfMiniNumbers.begin()+j+1);
                j--;
            }
        }
        i++;
    }
    cout<<listOfMaxiNumbers[0]<<endl;
    cout<<listOfMiniNumbers[0]<<endl;
    co2Scrubber = stoi(listOfMiniNumbers[0],0,2);
    oxygenGenerator = stoi(listOfMaxiNumbers[0],0,2);
    cout<<co2Scrubber<<" "<<oxygenGenerator<<endl;
    cout<<oxygenGenerator*co2Scrubber<<endl;
}
