# Learn Statistical Models in R

Dataset: Passenger car mileage (04cars.rda)
-  We consider the 04cars dataset (See description online)
- For now, we focus on the following variables:
1. mpg Highway gas consumption (miles per gallon)
2. hp Horsepower
3. wt Weight (pounds)
4. len Length (inches)
5. wd Width (inches)
- Goal: Predict a car’s gas consumption based on these characteristics
- Graphics: pairwise scatterplots and individual boxplots 

Dataset: Passenger car mileage (04cars.version2.rda)
- We now focus on the following variables:
1. mpg : Highway MPG 
2. type : Vehicle Type
(1 = SportsCar, 2 = SportUtility, 3 = Wagon, 4 = Minivan, 5 = Pickup)
3. drive : Drive Train 
(0 = Front-Wheel, 1 = Rear-Wheel, 2 = All-Wheel)
4. cyl : Number of Cylinders
5. hp : Horsepower 
6. wt : Weight 
- Goal: Explain mpg as a function of the other variables

Dataset: Alcohol and tobacco expenses in Great Britain (alcoholtobacco.rda)
- This dataset contains the average weekly household spending, in British pounds, on tobacco products and alcoholic beverages for each of the 11 regions of Great Britain (1981).
- We fit a simple linear model using least squares
Tobacco = β0 + β1 Alcohol + Error. The t-test for β1 = 0 is not significant.
- We now fit the model without 11:Northern.Ireland, and the t-test for β1 = 0 is highly significant.
- Observation 11:Northern.Ireland is therefore influential. (The other observations do not have nearly the same impact.)

Dataset: Aircraft Damage dataset (AircraftDamage.rda)
- Consider the Aircraft Damage dataset taken from Applied Linear Regression (4th Edition) by Weisberg.
- This is a dataset on the result of strike missions during the Vietnam War with A-4 or A-6 aircrafts.
- The variables are:
1. y: is the number of locations where the aircraft was damaged
2. x1: indicates the type of plane (0 for A-4; 1 for A-6)
3. x2: is the bomb load in tons
4. x3: is the total months of aircrew experience
