# 1. Stwórz funkcję o nazwie kostka, która będzie symulować n rzutów kostką.
# Wskazówka: Użyj funkcji sample() do losowania liczby oczek od 1 do 6.
kostka = function(n){
  if (n>0){
    wynik = sample(1:6,n, replace=TRUE)
  } else if (n<=0){
    wynik = "brak rzutow"
  }
  return (wynik)
}
# 5. Stwórz funkcję będącą najprostszą wersją kalkulatora 
# (dodawanie, odejmowanie, mnożenie albo dzielenie dwóch liczb).
kalkulator = function (liczba1,liczba2,dzialanie){
  if (dzialanie=="dodawanie"){
    wynik=liczba1+liczba2
  } else if (dzialanie=="odejmowanie"){
    wynik=liczba1-liczba2
  } else if (dzialanie=="mnozenie"){
    wynik=liczba1*liczba2
  } else if (dzialanie=="dzielenie" && liczba2!=0){
    wynik=liczba1/liczba2
  } else if (dzialanie=="dzielenie" && liczba2==0){
    wynik="nie mozna wykonac dzielenia"
  } else {
    wynik="nieznane dzialanie"
  } 
  return(wynik)
}
