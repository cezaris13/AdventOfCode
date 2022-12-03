import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class day2 {
    enum Game {
        Rock,
        Paper,
        Scissors
    }

    enum Outcome {
        Lose,
        Draw,
        Win
    }

    public static void main(String[] args) {
        int bestScore = 0;
        int bestScorePt2 = 0;
        try {
            File myObj = new File("input");
            Scanner myReader = new Scanner(myObj);
            while (myReader.hasNextLine()) {
                String data = myReader.nextLine();
                String[] inputOutputOfGame = data.split("\\s+");
                Game player1 = inputOutputOfGame[0].contains("A") ? Game.Rock : inputOutputOfGame[0].contains("B") ? Game.Paper : Game.Scissors;
                Game player2 = inputOutputOfGame[1].contains("X") ? Game.Rock : inputOutputOfGame[1].contains("Y") ? Game.Paper : Game.Scissors;
                Outcome gameOutcome = inputOutputOfGame[1].contains("X") ? Outcome.Lose : inputOutputOfGame[1].contains("Y") ? Outcome.Draw : Outcome.Win;
                bestScore+=getOneGameScore(player1, player2);
                bestScorePt2+=getOneGameScorePt2(player1, gameOutcome);
            }
            myReader.close();
        } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }

        System.out.println(bestScore);
        System.out.println(bestScorePt2);
    }

    public static int getOneGameScore(Game player1, Game player2){// part 1
        int score = player2.ordinal() + 1;
        if(player1.equals(player2)){
            score+=3;
        }
        if(player1 == Game.Rock){
            if(player2 == Game.Paper){
                score+=6;
            }
        }
        if(player1 == Game.Paper){
            if(player2 == Game.Scissors){
                score+=6;
            }
        }
        if(player1 == Game.Scissors){
            if(player2 == Game.Rock){
                score+=6;
            }
        }
        return score;
    }

    public static int getOneGameScorePt2(Game player1, Outcome gameOutcome){// part 2
        int score = 0;
        if(player1 == Game.Rock){
            if(gameOutcome == Outcome.Lose){
                score+=Game.Scissors.ordinal()+1;
            }
            if(gameOutcome == Outcome.Draw){
                score+=Game.Rock.ordinal()+1+3;
            }
            if(gameOutcome == Outcome.Win){
                score+=Game.Paper.ordinal()+1+6;
            }

        }
        if(player1 == Game.Paper){
            if(gameOutcome == Outcome.Lose){
                score+=Game.Rock.ordinal()+1;
            }
            if(gameOutcome == Outcome.Draw){
                score+=Game.Paper.ordinal()+1+3;
            }
            if(gameOutcome == Outcome.Win){
                score+=Game.Scissors.ordinal()+1+6;
            }
        }
        if(player1 == Game.Scissors){
            if(gameOutcome == Outcome.Lose){
                score+=Game.Paper.ordinal()+1;
            }
            if(gameOutcome == Outcome.Draw){
                score+= Game.Scissors.ordinal()+1+3;
            }
            if(gameOutcome == Outcome.Win){
                score+=Game.Rock.ordinal()+1+6;
            }
        }
        return score;
    }
}
