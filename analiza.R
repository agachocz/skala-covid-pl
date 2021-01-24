# Biblioteki
library(dplyr)
library(tidyr)
library(ggplot2)

# Wczytanie danych
# Źródło: baza danych Covid-19 Michała Rogalskiego
# Link: https://docs.google.com/spreadsheets/u/1/d/1ierEhD6gcq51HAm433knjnVwey4ZE5DCnu1bW7PRG3E/htmlview?fbclid=IwAR0oqN4ikk1flMvFbb0ecLOKGpkHo7LCYj75N6Z1vc4n_kuhtI214dqN5Ho
# Wybrane zmienne z arkusza Testy

dane <- read.table("testy.csv", sep = ",", header = T)
dane$data <- as.Date(dane$data, format = "%d.%m.%Y")

# Wstępna analiza danych, statystyki opisowe


# Dane z pierwszych dwóch miesięcy nie pokazują l. przetestowanych osób,
# tylko liczbę wykonanych testów
testy_vs_osoby <- dane %>% select(nowe_testowane, nowe_testy) %>%
  mutate(ratio = nowe_testowane/nowe_testy)

# Stosunek wynosi najczęściej ok. 90%
# Wartości powyżej 1 to prawd. błąd raportowania (opóźnienia w naliczaniu, itd.)
hist(testy_vs_osoby$ratio)



# Procentowe wartości

dane <- dane %>% mutate(procent_pozytywnych = nowe_pozytywne/nowe_testowane)

# Dzienna liczba testowanych osób,
# osób z pozytywnym wynikiem,
# oraz odsetek osób z pozytywnym wynikiem wśród testowanych osób

dane %>% select(data, nowe_testowane, nowe_pozytywne) %>%
  pivot_longer(cols = c(nowe_testowane, nowe_pozytywne), names_to = "typ", values_to = "dane") %>% 
  mutate(typ = factor(typ, levels = c("nowe_testowane", "nowe_pozytywne"))) %>%
  ggplot() + geom_ribbon(aes(x = data, ymin = 0, ymax = dane, fill = typ)) +
  scale_fill_manual(values = c("#055659", "#FDAB07"))

dane %>% ggplot(aes(x = data, y = procent_pozytywnych)) + geom_line()

# Proporcje testów w różnych dniach tygodnia