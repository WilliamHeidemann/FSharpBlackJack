namespace BlackJack

open System
open System.Threading

module GamePlay =
    type Card = string
    type Hand = Card list
    type Outcome =
        | Win
        | Loss
        | Draw
    
    let random = Random()

    let NextCard() : Card =
        let cards = ["A"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "J"; "Q"; "K"]
        let cardIndex = random.Next(cards.Length)
        let card = List.item cardIndex cards
        let suits = ["♠"; "♥"; "♦"; "♣"]
        let suitIndex = random.Next(suits.Length)
        let suit = List.item suitIndex suits
        card + suit

    let CardValue (card:Card) : int =
        let number = card.[0]
        match number with
        | 'A' -> 11 // Could be 1
        | '2' -> 2
        | '3' -> 3
        | '4' -> 4
        | '5' -> 5
        | '6' -> 6
        | '7' -> 7
        | '8' -> 8
        | '9' -> 9
        | _ -> 10

    let HandValue (hand:Hand) : int =
        List.sumBy CardValue hand
        
    let Print (str:string) (hand:Hand) =
        Console.WriteLine (str + (String.concat "" hand) + " (" + (HandValue hand).ToString() + ")")
    
    let rec PlaceBet (pot:int) : int =
        Console.WriteLine ("Your pot size is " + pot.ToString())
        Console.WriteLine "Place your bet"
        let input = Console.ReadLine()
        match Int32.TryParse input with
        | true, number -> if number <= pot && number > 0 then number else PlaceBet pot
        | false, _ -> PlaceBet pot
    
    let Hit (hand:Hand) : Hand =
        hand @ [NextCard()]
        
    let rec DealerHit (dealerHand:Hand) : Hand =
        Print "Dealer's Hand: " dealerHand
        Thread.Sleep(1000)
        match (HandValue dealerHand) with
        | value when value < 17 -> DealerHit (Hit dealerHand)
        | _ -> dealerHand
    
    let PlayDealer (playerHand:Hand) =
        let dealerHand = DealerHit ([NextCard()] @ [NextCard()])
        let dealerValue = HandValue dealerHand
        let playerValue = HandValue playerHand
        match dealerValue with
        | value when value > 21 ->
            Console.WriteLine "You won!"
            Outcome.Win
        | value when value < playerValue ->
            Console.WriteLine "You won!"
            Outcome.Win
        | value when value > playerValue ->
            Console.WriteLine "You lost!"
            Outcome.Loss
        | _ ->
            Console.WriteLine "Draw!"
            Outcome.Draw
    
    let rec PreRound (hand:Hand) =
        if (HandValue hand) > 21 then
            Print "Player's Hand: " hand
            Console.WriteLine "You lost!"
            Outcome.Loss
        else Round hand
    and Round (hand:Hand) =
        Print "Player's Hand: " hand
        if (HandValue hand) = 21 then PlayDealer hand
        else 
            Console.WriteLine "Press h to hit or s to stand"
            let input = Console.ReadKey(true)
            match input.KeyChar with
            | 'h' -> Hit hand |> PreRound
            | 's' -> PlayDealer hand
            | _ -> Round hand
            
            
    let rec Play (pot:int) =
        Thread.Sleep(1000)
        Console.Clear()
        if pot <= 0
        then Console.WriteLine "Game Over"
        else
            let bet = PlaceBet pot
            let (startingHand:Hand) = [NextCard()] @ [NextCard()]
            let outcome = Round startingHand
            match outcome with
            | Win -> Play (pot + bet)
            | Loss -> Play (pot - bet)
            | Draw -> Play pot
        
    let Game =
        Console.WriteLine "Game Started"
        let buyIn = 10
        Play buyIn
        
    Game