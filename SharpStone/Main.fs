(*
 * Sharpstone: a tiny card game simulator
 *
 * Written as project exam template for Computer Science, Laboratorio di Programmazione
 * Freely adapted from Heartstone (TM) by Blizzard Entertainment, Inc.
 *
 * (C) 2016 Alvise Spanò @ DAIS, Università Ca' Foscari, Venezia
 *)

module LabProg2016.Sharpstone

//#indent "off"

#if INTERACTIVE
#r "System.Runtime.Serialization.dll"
#endif

open System
open System.IO
open System.Runtime.Serialization
open System.Text

// globals
//

let rand = new Random (23)    // remove seed argument for making randomness indeterministic

/// Generate a random integer within the interval (a, b) inclusively.
let rnd_int a b = rand.Next (a, b + 1) 


// type definitions
//

/// Defines the card type.
[< DataContract; StructuralEquality; NoComparison >]
type card = {
    [< field: DataMember(Name = "id") >] id : string
    [< field: DataMember(Name = "name") >] name : string
    [< field: DataMember(Name = "cost") >] cost : int
    [< field: DataMember(Name = "type") >] typee : string
    [< field: DataMember(Name = "attack") >] attack : int
    [< field: DataMember(Name = "health") >] mutable health : int
}
with
    override c.ToString () = sprintf "%s [Id:%s  Atk:%d  HP:%d]" c.name c.id c.attack c.health

/// Deck type alias.
type deck = card list

/// Defined the player type.
[< StructuralEquality; NoComparison >]
type player = {
    name : string
    mutable life : int
    mutable deck : deck
}
with
    override p.ToString () = sprintf "%s [Life:%d  Deck:%d]" p.name p.life p.deck.Length


// JSON stuff
//

/// Convert a JSON string into a typed value.
let unjson<'t> (input : string) : 't =  
    use ms = new MemoryStream(ASCIIEncoding.Default.GetBytes(input)) 
    let obj = (new Json.DataContractJsonSerializer(typeof<'t>)).ReadObject(ms) 
    obj :?> 't

/// Parse a JSON deck given the filename.
let parse_deck (filename : string) = 
    use fstr = File.OpenRead filename
    use rd = new StreamReader (fstr)
    printfn "Parsing JSON file \"%s\"..." fstr.Name
    rd.ReadToEnd () |> unjson<card[]> |> Array.toList


// printers
//

/// Prints the turn number header. Call this function at the beginning of each turn.
let print_turn_begin (turn : int) = printfn "\nTurn %d:" turn

/// Prints the status of the 2 players. Call this function at the end of each turn.
let print_turn_end (p1 : player, p2 : player) = printfn "\t%O\n\t%O" p1 p2

/// Prints the information of 2 cards fighting. Call this function at each turn when both players have a card.
let print_turn_2cards (c1 : card, c2 : card) = printfn "%O VS %O" c1 c2

/// Prints the information of 1 card fighting against a player with no cards. Call this function at each turn when only 1 players have a card.
let print_turn_1card (p : player, c : card) = printfn "* %O VS player %O" c p

/// Prints the information of 2 players when both have no cards. Call this function at each turn no cards have been drawn.
let print_turn_no_cards (p1 : player, p2 : player) = printfn "* Both %O and %O have no cards" p1 p2

/// Prints the information of a dead cards. Call this function when a card dies.
let print_card_death (c : card) = printfn "+ %O died (%d overkill)" { c with health = 0 } -c.health


// Aux functions

/// Checks if the passed deck is empty or not
let is_empty (d : deck) : bool = if d = [] then true else false

/// Returns a card score. The bigger the value the better the card
let card_score (card : card) : float = float(card.attack) / float(card.health)

let rec filter_by_mana (m : int) (d : deck) : deck = match d with
                                                                         [] -> []
                                                                         |x::xs -> if x.cost <= m then x::filter_by_mana m xs
                                                                                    else filter_by_mana m xs

/// Returns the max score value of a deck
let rec max_score (score : float) (d: deck) : float = match d with 
                                                                     [] -> score
                                                                     |x::xs -> if (card_score x > score) then max_score (card_score x) xs
                                                                                else max_score score xs

/// Returns all cards with the highest score
let rec filter_by_score (score : float) (d : deck) : deck = match d with 
                                                                               [] -> []
                                                                               |x::xs -> if (card_score x < score) then filter_by_score score xs
                                                                                         else x::filter_by_score score xs

/// Returns a card from position x from a non-empty deck
let rec get_card_in_pos (x : int) (d : deck) : card = match x with 
                                                            1 -> d.Head
                                                            |_ -> get_card_in_pos (x-1) d.Tail

/// Randomly gets a card from a card list
let get_minion (d : deck): card = if d.Length = 1 then d.Head else get_card_in_pos (rnd_int 1 (d.Length)) d

/// Recursive function that fiters a given deck into a new one with minions only 
let rec filter_deck (d : deck) : deck = match d with
                                                              [] -> []
                                                              |x::xs -> if x.typee = "MINION" && x.attack > 0 && x.health > 0 then x::filter_deck xs
                                                                         else filter_deck xs
                                                                                                                                           
/// Returns a card chosen by the amount of mana
let draw_card (mana : int) (d : deck) : card = if is_empty (filter_by_mana mana d) then {id="";name="";cost=0;typee="";attack=0;health=0}
                                                    else get_minion (filter_by_score (max_score 0.0 (filter_by_mana mana d)) (filter_by_mana mana d))

/// Removes a card from a deck
let rec remove_card (c:card) (d: deck) : deck = match d with
                                                [] -> []  
                                                |x::xs -> if x.id = c.id then xs
                                                          else x::remove_card c (xs)

/// Updates cards life 
let rec update_card_life (c : card ) (d : deck) : deck = match d with
                                                                [] -> []
                                                                |x::xs ->if x.id = c.id then x.health <- c.health
                                                                         x::update_card_life c xs
                                                                          


// combat mechanics
//

// !!! YOU MUST IMPLEMENT THIS !!!
let fight (deck1 : deck) (deck2 : deck) : player * player * int =
    let p1 = { name ="P1"; life = 30; deck = deck1 }    // dummy players
    let p2 = { name ="P2"; life = 30; deck = deck2 }
    let mutable turn = 1
    let mutable quit = is_empty p1.deck && is_empty p2.deck
    while not quit && p1.life > 0 && p2.life > 0 do 
            print_turn_begin turn
            let mana = if turn > 10 then 10 else turn 
            let c1 = draw_card mana p1.deck
            let c2 = draw_card mana p2.deck
            if not (c1.attack = 0) && not (c2.attack = 0) then print_turn_2cards (c1,c2)
            else if (c1.attack = 0) && not (c2.attack = 0) then print_turn_1card(p1,c2)
            else if (c2.attack = 0) && not (c1.attack = 0) then print_turn_1card(p2,c1)
            else print_turn_no_cards (p1,p2)

            // Decreses player life when one or more minions are missing
            if (c1.attack = 0) && not (c2.attack = 0) then p1.life <- p1.life - c2.attack
            if (c2.attack = 0) && not (c1.attack = 0) then p2.life <- p2.life - c1.attack

            // Updates cards life after the battle
            if not (c1.attack = 0) && not (c2.attack = 0) then 
                   c1.health <- c1.health - c2.attack
                   c2.health <- c2.health - c1.attack

            // Decreases players life in case of overkill
            if not (c1.attack = 0) && not (c2.attack = 0) then 
                  if (c1.health < 0) then p1.life <- p1.life - abs(c1.health)
                  if (c2.health < 0) then p2.life <- p2.life - abs(c2.health)
                 
            // Updates card life in decks after the battle
            if not (c1.attack = 0) && not (c2.attack = 0) then 
                   p1.deck <- update_card_life c1 p1.deck
                   p2.deck <- update_card_life c2 p2.deck

            // Removes card from the game when killed    
            if not (c1.attack = 0) && not (c2.attack = 0) then
                  if (c1.health <= 0) then p1.deck <- remove_card c1 p1.deck 
                  if (c2.health <= 0) then p2.deck <- remove_card c2 p2.deck

            // Print overkill info
            if not (c1.attack = 0) && not (c2.attack = 0) then 
                  if (c1.health <= 0) then print_card_death(c1)
                  if (c2.health <= 0) then print_card_death(c2)

            print_turn_end(p1,p2)
            turn <- turn+1
            quit <- is_empty p1.deck && is_empty p2.deck
    p1, p2, (turn-1)

// main code
//

[< EntryPoint >]
let main argv =
    let code =
        try
            if argv.Length <> 2 then
                printfn "Usage: Sharpstone <DECK1> <DECK2>"
                0
            else
                let p filename = parse_deck filename    // function for parsing a JSON file defining a deck as a list of cards
                let d1 = filter_deck( p argv.[0] )                   // parse the first argument of the executable (DECK1)
                let d2 = filter_deck( p argv.[1] )                  // parse the second argument of the executable (DECK2)
                let p1, p2, turn as r = fight d1 d2                 // tutta la tripla (p1, p2, turn) si chiama ora r grazie a AS
                // print final result - ossia vita giocatore1, giocatore2 e il numero di turni che ci ha messo la partita a terminare ossia turn. 
                //Attenzione a verificare la vita del giocatore alla fine del turno altrimenti si rischia di conteggiare un turno in più...credo
                printfn "\nResult:\n\t%d Turns\n\t%O\n\t%O\n\tHash: %X" turn p1 p2 (r.GetHashCode ())
                0

        with e -> printfn "Uncaught exception: %O" e; 1

    #if DEBUG   
    printfn "\n\nPress any key to exit..."
    Console.ReadKey () |> ignore
    #endif
    code