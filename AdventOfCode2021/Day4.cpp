#include <bits/stdc++.h>

using namespace std;

vector<vector<int>> readBingoBoard(){
  cin.ignore(80,'\n');
  vector<vector<int>> newBingoBoard;
  for(int i=0;i<5;i++){
    vector<int> bingoRow;
    for(int j=0;j<5;j++){
      int input;
      cin>>input;
      bingoRow.push_back(input);
    }
    newBingoBoard.push_back(bingoRow);
  }
  return newBingoBoard;
}

vector<vector<bool>> emptyBingoBoard(){
    vector<vector<bool>> newBingoBoard;
    for(int i=0;i<5;i++){
        vector<bool> bingoRow;
        for(int j=0;j<5;j++){
            bool input=false;;
            bingoRow.push_back(input);
        }
        newBingoBoard.push_back(bingoRow);
    }
    return newBingoBoard;
}

bool checkBoardForBingo(vector<vector<bool>> markedBingoBoard){
  for(int i=0;i<5;i++){
    bool allMarked=true;
    for(int j=0;j<5;j++){
      if(!markedBingoBoard[i][j])
        allMarked=false;
    }
    if(allMarked)
      return true;
    allMarked=true;
    for(int j=0;j<5;j++){
      if(!markedBingoBoard[j][i])
        allMarked=false;
    }
    if(allMarked)
      return true;
  }
  return false;
}

bool markBingoBoard(vector<vector<int>> bingoNumbers, vector<vector<bool>> &markedNumbers,int number)
{
  for(int i=0;i<5;i++){
    for(int j=0;j<5;j++){
      if(bingoNumbers[i][j]==number)
      {
        markedNumbers[i][j]=true;
        bool verdict = checkBoardForBingo(markedNumbers);
        if(verdict){
          return true;
        }
      }
    }
  }
  return false;
}
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
int scoreOfUnmarked(vector<vector<bool>> markedBingo,vector<vector<int>> bingo){
  int score=0;
  for(int i=0;i<5;i++){
    for(int j=0;j<5;j++){
      if(!markedBingo[i][j]){
        score+=bingo[i][j];
      }
    }
  }
    cout<<endl;
  return score;
}
// int main()
// {
//   freopen("input.txt","r",stdin);
//   string inputNumbers;
//   cin>>inputNumbers;
//   vector<vector<vector<int>>> bingoBoards;
//   vector<vector<vector<bool>>> markedBingoBoards;
//   while(!cin.eof()){
//    vector<vector<int>> temp =readBingoBoard();
//    vector<vector<bool>> temp1 =emptyBingoBoard();
//    bingoBoards.push_back(temp);
//    markedBingoBoards.push_back(temp1);
//   }
//   vector<int> numbersList = returnListOfInts(inputNumbers);
//   for(int i=0;i<numbersList.size();i++){
//     for(int j=0;j<bingoBoards.size();j++){
//       if(markBingoBoard(bingoBoards[j],markedBingoBoards[j],numbersList[i])){
//         int score=scoreOfUnmarked(markedBingoBoards[j],bingoBoards[j]);
//         cout<<score<<endl;
//         cout<<numbersList[i]<<endl;
//         cout<<score*numbersList[i];
//         j=bingoBoards.size();
//         i=numbersList.size();
//       }
//     }
//   }
//  }
int main()
{// last bingo board score and etc.
  freopen("input.txt","r",stdin);
  string inputNumbers;
  cin>>inputNumbers;
  vector<vector<vector<int>>> bingoBoards;
  vector<vector<vector<bool>>> markedBingoBoards;
  while(!cin.eof()){
   vector<vector<int>> temp =readBingoBoard();
   vector<vector<bool>> temp1 =emptyBingoBoard();
   bingoBoards.push_back(temp);
   markedBingoBoards.push_back(temp1);
  }

  vector<int> numbersList = returnListOfInts(inputNumbers);
  for(int i=0;i<numbersList.size();i++){
    for(int j=0;j<bingoBoards.size()-1;j++){
      if(markBingoBoard(bingoBoards[j],markedBingoBoards[j],numbersList[i])){
        cout<<bingoBoards.size()<<endl;
        if(bingoBoards.size()==2)
        {
          int score=scoreOfUnmarked(markedBingoBoards[j],bingoBoards[j]);
          cout<<score<<endl;
          cout<<numbersList[i]<<endl;
          cout<<score*numbersList[i];
        }
       bingoBoards.erase(bingoBoards.begin()+j,bingoBoards.begin()+j+1);
       markedBingoBoards.erase(markedBingoBoards.begin()+j,markedBingoBoards.begin()+j+1);
       j--;
      }
    }
  }
 }
