myDataFrame <- read.csv(file="f:/Data Analytics/Assingment 2/pokemon.csv", header=TRUE, sep=",")

normalize <- function(x)
{
	z=x
 if(min(x)<max(x)){ 
  z=(x - min(x)) / (max(x) - min(x))
 }
 return(z)
}

#finding subsets for all numeric values
hp_grass <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Grass" , select = c(HP)))
attack_grass <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Grass" , select = c(Attack)))
defence_grass <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Grass" , select = c(Defense)))
sp_attack_grass <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Grass" , select = c(Sp_Atk)))
sp_defence_grass <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Grass" , select = c(Sp_Def)))
speed_grass <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Grass" , select = c(Speed)))


hp_fire <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fire" , select = c(HP)))
attack_fire <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fire" , select = c(Attack)))
defence_fire <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fire" , select = c(Defense)))
sp_attack_fire <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fire" , select = c(Sp_Atk)))
sp_defence_fire <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fire" , select = c(Sp_Def)))
speed_fire <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fire" , select = c(Speed)))

#normalizing the values and finding the mean
HP_grass <- apply(hp_grass,2,mean)
Attack_grass <- apply(attack_grass,2,mean)
Defence_grass <- apply(defence_grass,2,mean)
Sp_Atk_grass <- apply(sp_attack_grass,2,mean)
Sp_Def_grass <- apply(sp_defence_grass,2,mean)
Speed_grass <- apply(speed_grass,2,mean)

HP_fire <- apply(hp_fire,2,mean)
Attack_fire <- apply(attack_fire,2,mean)
Defence_fire <- apply(defence_fire,2,mean)
Sp_Atk_fire <- apply(sp_attack_fire,2,mean)
Sp_Def_fire <- apply(sp_defence_fire,2,mean)
Speed_fire <- apply(speed_fire,2,mean)

#applying distance formula to find the distance between the means
distance <- (((HP_grass - HP_fire)^2 + (Attack_grass - Attack_fire)^2 + (Defence_grass - Defence_fire)^2 + (Sp_Atk_grass - Sp_Atk_fire)^2 + (Sp_Def_grass - Sp_Def_fire)^2 + (Speed_grass - Speed_fire)^2)^0.5)

distance

#plotting line graph of the mean of the normalizedvalues of these attributes

#finding the normalized mean for all the 18 types
hp_water <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Water" , select = c(HP)))
attack_water <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Water" , select = c(Attack)))
defence_water <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Water" , select = c(Defense)))
sp_attack_water <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Water" , select = c(Sp_Atk)))
sp_defence_water <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Water" , select = c(Sp_Def)))
speed_water <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Water" , select = c(Speed)))

HP_water <- apply(hp_water,2,mean)
Attack_water <- apply(attack_water,2,mean)
Defence_water <- apply(defence_water,2,mean)
Sp_Atk_water <- apply(sp_attack_water,2,mean)
Sp_Def_water <- apply(sp_defence_water,2,mean)
Speed_water <- apply(speed_water,2,mean)

hp_bug <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Bug" , select = c(HP)))
attack_bug <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Bug" , select = c(Attack)))
defence_bug <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Bug" , select = c(Defense)))
sp_attack_bug <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Bug" , select = c(Sp_Atk)))
sp_defence_bug <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Bug" , select = c(Sp_Def)))
speed_bug <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Bug" , select = c(Speed)))

HP_bug <- apply(hp_bug,2,mean)
Attack_bug <- apply(attack_bug,2,mean)
Defence_bug <- apply(defence_bug,2,mean)
Sp_Atk_bug <- apply(sp_attack_bug,2,mean)
Sp_Def_bug <- apply(sp_defence_bug,2,mean)
Speed_bug <- apply(speed_bug,2,mean)

hp_normal <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Normal" , select = c(HP)))
attack_normal <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Normal" , select = c(Attack)))
defence_normal <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Normal" , select = c(Defense)))
sp_attack_normal <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Normal" , select = c(Sp_Atk)))
sp_defence_normal <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Normal" , select = c(Sp_Def)))
speed_normal <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Normal" , select = c(Speed)))

HP_normal <- apply(hp_normal,2,mean)
Attack_normal <- apply(attack_normal,2,mean)
Defence_normal <- apply(defence_normal,2,mean)
Sp_Atk_normal <- apply(sp_attack_normal,2,mean)
Sp_Def_normal <- apply(sp_defence_normal,2,mean)
Speed_normal <- apply(speed_normal,2,mean)

hp_poison <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Poison" , select = c(HP)))
attack_poison <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Poison" , select = c(Attack)))
defence_poison <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Poison" , select = c(Defense)))
sp_attack_poison <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Poison" , select = c(Sp_Atk)))
sp_defence_poison <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Poison" , select = c(Sp_Def)))
speed_poison <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Poison" , select = c(Speed)))

HP_poison <- apply(hp_poison,2,mean)
Attack_poison <- apply(attack_poison,2,mean)
Defence_poison <- apply(defence_poison,2,mean)
Sp_Atk_poison <- apply(sp_attack_poison,2,mean)
Sp_Def_poison <- apply(sp_defence_poison,2,mean)
Speed_poison <- apply(speed_poison,2,mean)

hp_electric <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Electric" , select = c(HP)))
attack_electric <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Electric" , select = c(Attack)))
defence_electric <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Electric" , select = c(Defense)))
sp_attack_electric <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Electric" , select = c(Sp_Atk)))
sp_defence_electric <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Electric" , select = c(Sp_Def)))
speed_electric <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Electric" , select = c(Speed)))

HP_electric <- apply(hp_electric,2,mean)
Attack_electric <- apply(attack_electric,2,mean)
Defence_electric <- apply(defence_electric,2,mean)
Sp_Atk_electric <- apply(sp_attack_electric,2,mean)
Sp_Def_electric <- apply(sp_defence_electric,2,mean)
Speed_electric <- apply(speed_electric,2,mean)

hp_ground <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ground" , select = c(HP)))
attack_ground <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ground" , select = c(Attack)))
defence_ground <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ground" , select = c(Defense)))
sp_attack_ground <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ground" , select = c(Sp_Atk)))
sp_defence_ground <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ground" , select = c(Sp_Def)))
speed_ground <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ground" , select = c(Speed)))

HP_ground <- apply(hp_ground,2,mean)
Attack_ground <- apply(attack_ground,2,mean)
Defence_ground <- apply(defence_ground,2,mean)
Sp_Atk_ground <- apply(sp_attack_ground,2,mean)
Sp_Def_ground <- apply(sp_defence_ground,2,mean)
Speed_ground <- apply(speed_ground,2,mean)

hp_fairy <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fairy" , select = c(HP)))
attack_fairy <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fairy" , select = c(Attack)))
defence_fairy <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fairy" , select = c(Defense)))
sp_attack_fairy <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fairy" , select = c(Sp_Atk)))
sp_defence_fairy <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fairy" , select = c(Sp_Def)))
speed_fairy <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fairy" , select = c(Speed)))

HP_fairy <- apply(hp_fairy,2,mean)
Attack_fairy <- apply(attack_fairy,2,mean)
Defence_fairy <- apply(defence_fairy,2,mean)
Sp_Atk_fairy <- apply(sp_attack_fairy,2,mean)
Sp_Def_fairy <- apply(sp_defence_fairy,2,mean)
Speed_fairy <- apply(speed_fairy,2,mean)

hp_fighting <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fighting" , select = c(HP)))
attack_fighting <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fighting" , select = c(Attack)))
defence_fighting <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fighting" , select = c(Defense)))
sp_attack_fighting <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fighting" , select = c(Sp_Atk)))
sp_defence_fighting <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fighting" , select = c(Sp_Def)))
speed_fighting <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Fighting" , select = c(Speed)))

HP_fighting <- apply(hp_fighting,2,mean)
Attack_fighting <- apply(attack_fighting,2,mean)
Defence_fighting <- apply(defence_fighting,2,mean)
Sp_Atk_fighting <- apply(sp_attack_fighting,2,mean)
Sp_Def_fighting <- apply(sp_defence_fighting,2,mean)
Speed_fighting <- apply(speed_fighting,2,mean)

hp_psychic <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Psychic" , select = c(HP)))
attack_psychic <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Psychic" , select = c(Attack)))
defence_psychic <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Psychic" , select = c(Defense)))
sp_attack_psychic <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Psychic" , select = c(Sp_Atk)))
sp_defence_psychic <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Psychic" , select = c(Sp_Def)))
speed_psychic <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Psychic" , select = c(Speed)))

HP_psychic <- apply(hp_psychic,2,mean)
Attack_psychic <- apply(attack_psychic,2,mean)
Defence_psychic <- apply(defence_psychic,2,mean)
Sp_Atk_psychic <- apply(sp_attack_psychic,2,mean)
Sp_Def_psychic <- apply(sp_defence_psychic,2,mean)
Speed_psychic <- apply(speed_psychic,2,mean)

hp_rock <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Rock" , select = c(HP)))
attack_rock <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Rock" , select = c(Attack)))
defence_rock <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Rock" , select = c(Defense)))
sp_attack_rock <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Rock" , select = c(Sp_Atk)))
sp_defence_rock <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Rock" , select = c(Sp_Def)))
speed_rock <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Rock" , select = c(Speed)))

HP_rock <- apply(hp_rock,2,mean)
Attack_rock <- apply(attack_rock,2,mean)
Defence_rock <- apply(defence_rock,2,mean)
Sp_Atk_rock <- apply(sp_attack_rock,2,mean)
Sp_Def_rock <- apply(sp_defence_rock,2,mean)
Speed_rock <- apply(speed_rock,2,mean)

hp_flying <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Flying" , select = c(HP)))
attack_flying <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Flying" , select = c(Attack)))
defence_flying <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Flying" , select = c(Defense)))
sp_attack_flying <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Flying" , select = c(Sp_Atk)))
sp_defence_flying <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Flying" , select = c(Sp_Def)))
speed_flying <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Flying" , select = c(Speed)))

HP_flying <- apply(hp_flying,2,mean)
Attack_flying <- apply(attack_flying,2,mean)
Defence_flying <- apply(defence_flying,2,mean)
Sp_Atk_flying <- apply(sp_attack_flying,2,mean)
Sp_Def_flying <- apply(sp_defence_flying,2,mean)
Speed_flying <- apply(speed_flying,2,mean)

hp_ghost <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ghost" , select = c(HP)))
attack_ghost <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ghost" , select = c(Attack)))
defence_ghost <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ghost" , select = c(Defense)))
sp_attack_ghost <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ghost" , select = c(Sp_Atk)))
sp_defence_ghost <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ghost" , select = c(Sp_Def)))
speed_ghost <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ghost" , select = c(Speed)))

HP_ghost <- apply(hp_ghost,2,mean)
Attack_ghost <- apply(attack_ghost,2,mean)
Defence_ghost <- apply(defence_ghost,2,mean)
Sp_Atk_ghost <- apply(sp_attack_ghost,2,mean)
Sp_Def_ghost <- apply(sp_defence_ghost,2,mean)
Speed_ghost <- apply(speed_ghost,2,mean)

hp_dragon <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Dragon" , select = c(HP)))
attack_dragon <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Dragon" , select = c(Attack)))
defence_dragon <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Dragon" , select = c(Defense)))
sp_attack_dragon <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Dragon" , select = c(Sp_Atk)))
sp_defence_dragon <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Dragon" , select = c(Sp_Def)))
speed_dragon <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Dragon" , select = c(Speed)))

HP_dragon <- apply(hp_dragon,2,mean)
Attack_dragon <- apply(attack_dragon,2,mean)
Defence_dragon <- apply(defence_dragon,2,mean)
Sp_Atk_dragon <- apply(sp_attack_dragon,2,mean)
Sp_Def_dragon <- apply(sp_defence_dragon,2,mean)
Speed_dragon <- apply(speed_dragon,2,mean)

hp_ice <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ice" , select = c(HP)))
attack_ice <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ice" , select = c(Attack)))
defence_ice <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ice" , select = c(Defense)))
sp_attack_ice <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ice" , select = c(Sp_Atk)))
sp_defence_ice <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ice" , select = c(Sp_Def)))
speed_ice <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Ice" , select = c(Speed)))

HP_ice <- apply(hp_ice,2,mean)
Attack_ice <- apply(attack_ice,2,mean)
Defence_ice <- apply(defence_ice,2,mean)
Sp_Atk_ice <- apply(sp_attack_ice,2,mean)
Sp_Def_ice <- apply(sp_defence_ice,2,mean)
Speed_ice <- apply(speed_ice,2,mean)

hp_dark <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Dark" , select = c(HP)))
attack_dark <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Dark" , select = c(Attack)))
defence_dark <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Dark" , select = c(Defense)))
sp_attack_dark <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Dark" , select = c(Sp_Atk)))
sp_defence_dark <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Dark" , select = c(Sp_Def)))
speed_dark <- normalize(subset(myDataFrame, myDataFrame$Type_1 == "Dark" , select = c(Speed)))

HP_dark <- apply(hp_dark,2,mean)
Attack_dark <- apply(attack_dark,2,mean)
Defence_dark <- apply(defence_dark,2,mean)
Sp_Atk_dark <- apply(sp_attack_dark,2,mean)
Sp_Def_dark <- apply(sp_defence_dark,2,mean)
Speed_dark <- apply(speed_dark,2,mean)


#choosing the points for all 18 points to be plotted on the line graph
grassGraphPoints <- c(HP_grass, Attack_grass, Defence_grass, Sp_Atk_grass, Sp_Def_grass,Speed_grass)
fireGraphPoints <- c(HP_fire, Attack_fire, Defence_fire, Sp_Atk_fire, Sp_Def_fire,Speed_fire)
waterGraphPoints <- c(HP_water, Attack_water, Defence_water, Sp_Atk_water, Sp_Def_water,Speed_water)
bugGraphPoints <- c(HP_bug, Attack_bug, Defence_bug, Sp_Atk_bug, Sp_Def_bug,Speed_bug)
normalGraphPoints <- c(HP_normal, Attack_normal, Defence_normal, Sp_Atk_normal, Sp_Def_normal,Speed_normal)
electricGraphPoints <- c(HP_electric, Attack_electric, Defence_electric, Sp_Atk_electric, Sp_Def_electric,Speed_electric)
groundGraphPoints <- c(HP_ground, Attack_ground, Defence_ground, Sp_Atk_ground, Sp_Def_ground,Speed_ground)
fairyGraphPoints <- c(HP_fairy, Attack_fairy, Defence_fairy, Sp_Atk_fairy, Sp_Def_fairy,Speed_fairy)
fightingGraphPoints <- c(HP_fighting, Attack_fighting, Defence_fighting, Sp_Atk_fighting, Sp_Def_fighting,Speed_fighting)
psychicGraphPoints <- c(HP_psychic, Attack_psychic, Defence_psychic, Sp_Atk_psychic, Sp_Def_psychic,Speed_psychic)
rockGraphPoints <- c(HP_rock, Attack_rock, Defence_rock, Sp_Atk_rock, Sp_Def_rock,Speed_rock)
flyingGraphPoints <- c(HP_flying, Attack_flying, Defence_flying, Sp_Atk_flying, Sp_Def_flying,Speed_flying)
ghostGraphPoints <- c(HP_ghost, Attack_ghost, Defence_ghost, Sp_Atk_ghost, Sp_Def_ghost,Speed_ghost)
dragonGraphPoints <- c(HP_dragon, Attack_dragon, Defence_dragon, Sp_Atk_dragon, Sp_Def_dragon,Speed_dragon)
iceGraphPoints <- c(HP_ice, Attack_ice, Defence_ice, Sp_Atk_ice, Sp_Def_ice,Speed_ice)
darkGraphPoints <- c(HP_dark, Attack_dark, Defence_dark, Sp_Atk_dark, Sp_Def_dark,Speed_dark)
steelGraphPoints <- c(HP_steel, Attack_steel, Defence_steel, Sp_Atk_steel, Sp_Def_steel,Speed_steel)

plot(grassGraphPoints, type = "o", col = "blue" , xlab = "Attribute", ylab = "Normalized Value of the Attribute", main = "Line Graph")
lines(fireGraphPoints, type = "o", col = "red")
lines(waterGraphPoints, type = "o", col = "green")
lines(bugGraphPoints, type = "o", col = "pink")
lines(normalGraphPoints, type = "o", col = "yellow")
lines(poisonGraphPoints, type = "o", col = "skyblue")
lines(electricGraphPoints, type = "o", col = "black")
lines(groundGraphPoints, type = "o", col = "brown")
lines(fairyGraphPoints, type = "o", col = "aliceblue")
lines(fightingGraphPoints, type = "o", col = "azure2")
lines(psychicGraphPoints, type = "o", col = "aquamarine")
lines(rockGraphPoints, type = "o", col = "beige")
lines(flyingGraphPoints, type = "o", col = "bisque3")
lines(ghostGraphPoints, type = "o", col = "orange")
lines(dragonGraphPoints, type = "o", col = "gray")
lines(iceGraphPoints, type = "o", col = "magenta")
lines(darkGraphPoints, type = "o", col = "cyan")
lines(steelGraphPoints, type = "o", col = "red")


#calculating skewness and kurtosis for weights and heights
wt <- subset(myDataFrame, select = c(Weight_kg))
ht <- subset(myDataFrame, select = c(Height_m))

install.packages("moments")
install.packages("e1071")
library(moments)
library(e1071)
kurtosis_wt <- kurtosis(wt, na.rm = FALSE)
kurtosis_ht <- kurtosis(ht, na.rm = FALSE)

skewness_wt <- skewness(wt)
skewness_ht <- skewness(ht)

print("Kurtosis Weights : ", kurtosis_wt)
print("Kurotsis Heights : ", kurtosis_ht)
print("Skewness Weights : ", skewness_wt)
print("Skewness Heights : ", skewness_ht)

#Scaling the values
scale_hp <- scale(subset(myDataFrame, select = c(HP)))
scale_attack <- scale(subset(myDataFrame, select = c(Attack)))
scale_defence <- scale(subset(myDataFrame, select = c(Defense)))
scale_SpAtk <- scale(subset(myDataFrame, select = c(Sp_Atk)))
scale_SpDef <- scale(subset(myDataFrame, select = c(Sp_Def)))
scale_speed <- scale(subset(myDataFrame, select = c(Speed)))
scale_total <- scale(subset(myDataFrame, select = c(Total)))
scale_ht <- scale(subset(myDataFrame, select = c(Height_m)))
scale_wt <- scale(subset(myDataFrame, select = c(Weight_kg)))

#plotting scaled values
plot(density(scale_hp, main = "Normalization of attributes"), col = 2)
lines(density(scale_attack), col = 3)
lines(density(scale_defense), col = 4)
lines(density(scale_SpAtk), col = 5)
lines(density(scale_SpDef), col = 6)
lines(density(scale_speed), col = 7)
lines(density(scale_total), col = 8)
lines(density(scale_ht), col = 9)
lines(density(scale_wt), col = 10)

 











