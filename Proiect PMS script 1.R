
# introducem matricea de trecere P
P <- matrix(c(0.9, 0.075, 0.025,
              0.15, 0.8, 0.05,
              0.25, 0.25, 0.5),
            nrow = 3,
            byrow = TRUE)
P # returneaza matricea P
class(P) # solicitam tipul obiectului P definit anterior
dim(P) # returneaza numarul de linii si coloane al matricei P

# definim un vector care sa contina starile lantului Markov
states <- c("Bull", "Bear", "Stagnant")
states

# adaugam etichete liniilor matricei P, etichetele fiind elementele vectorului starilor
rownames(P) <- states
P
# adaugam etichete stari pentru fiecare coloana
colnames(P) <- states
P

?rowSums() # help pentru functia care returneaza suma elementelor de pe fiecare linie
rowSums(P) # suma elementelor de pe fiecare linie a unei matrice stochastice trebuie sa fie egala cu 1

# introducem distributia initiala a lantului,
# adica distributia alfa a lui X0, care este o distributie uniforma pe spatiul starilor S
init <- c(0, 0, 1) #Piata debuteaza ca stagnant
init
sum(init) # intotdeauna egala cu 1

# asociem vectorului distributiei initiale etichetele starilor
names(init) <- states

init

sum(init)

# simularea traiectoriei pentru 31 de zile

markov <- function(init,matrice,n,labels) {
  # daca starile nu sunt precizate, atunci starile sunt numerotate 1,....,k
  if (missing(labels)) labels <- 1:length(init)
  # construieste un vector denumit simlist cu n+1 elemente, initializat cu toate elementele 0
  simlist <- numeric(n+1)
  # starile sunt etichetele 1, 2, ..., k, unde k este numarul de stari al lantului
  states <- 1:length(init)
  # primul element al vectorului simlist este o realizare sau simularea unei singure stari
  # din distributia lui X0, adica din distributia initiala a lantului
  simlist[1] <- sample(states,
                       1,
                       prob=init)
  # celelalte elemente ale vectorului simlist, adica simlist[2], ..., simlist[n+1]
  # reprezinta fiecare cate o realizare sau o simulare
  # din distributia de probabilitate data de linia simlist[i-1] a matricei P
  for (i in 2:(n+1)) 
  { simlist[i] <- sample(states,
                         1,
                         prob=matrice[simlist[i-1],]) }
  labels[simlist] # ataseaza starile corespunzatoare elementelor vectorului simlist
}


traiectorie <- markov(init,P,31,states)
traiectorie

# ultima pozitie, adica a 32-a, din sirul sau traiectoria simulata
# reprezinta starea pt X31, adica starea in care se gaseste peste n=31 zile
# piata care a inceput ca "Stagnant"
traiectorie[32] 

# distributia de frecvente absolute
z<-table(traiectorie)
z
barplot(z, main = "Bar plot frecvente absolute", col = c("Red","Green","Blue"))

# distributia de frecvente relative
table(traiectorie)/length(traiectorie)


# Verificam daca matricea P este regulara
P > 0 #adevarat
# Matricea P este regulara deoarece este pozitiva


# Aflam distributia stationara 
stationary <- function(matrice) {
  x = eigen(t(matrice))$vectors[,1]
  as.double(x/sum(x))
}


# Verificam daca distributia este intr-adevar stationara si verificam egalitatea
#  pi * P = pi
stationary(P)
round(stationary(P), digits = 2)
distr_stationara <- stationary(P)
distr_stationara
stationary(P) %*% P
stationary(P) %*% P == stationary(P)
round(stationary(P) %*% P, digits = 2) == round(stationary(P), digits = 2)



# Scriem matricea pi
lim_p <- matrix(c(0.62, 0.31, 0.06,
                  0.62, 0.31, 0.06,
                  0.62, 0.31, 0.06
                  ),
                nrow = 3,
                byrow = TRUE)
colnames(lim_p) <- states
rownames(lim_p) <- states
lim_p

# Simularea traiectoriei de 10 000 de ori

sim_total <- replicate(10000,
                       markov(init,P,31,states))
sim_total

class(sim_total) # obiect de tip matrice
dim(sim_total) # 32 linii, corespunzatoare momentelor de timp, si 10 000 coloane, pt fiecare stare


sim_total[32,] # linia 32 din matricea sim_total

# dupa 31 zile de la momentul initial, in ce stare se afla piata?
Q <- table(sim_total[32,]) # distributia de frecvente absolute
Q
barplot(Q, main = "Bar plot frecvente absolute", col = c("Red","Green","Blue"))
# care sunt proportiile starilor pietelor?
table(sim_total[32,])/10000 # distributia de frecvente relative



