#pragma once

/*
A Leach's Storm-petrel egg, sitting in a comfy burrow
*/ 
class Egg {
	
public:

	// Constructor
	Egg();					

	/*
	Egg Behavior, where the egg is incubated (moving towards hatch date) 
	or suffers neglect (moving further from hatch date and dying if 
	neglected too long)
	@param incubated is at least one parent incubating the egg?
	*/
	void eggDay(bool incubated);							

    // Setters
    void setNeglectMax(int neglectMax_) { this->neglectMax = neglectMax_; }
	void setEggCost(double eggCost_) { this->eggCost = eggCost_; }
	
	// Getters
	bool isAlive() { return this->alive; }
	bool isHatched() { return this->hatched; }

	int getIncubationDays() { return this->currDays; }
	double getMaxHatchDays() { return this-> maxHatchDays; }
	int getTotNeg() { return this->totNegCounter; }
	int getMaxNeg() { return this->maxNegCounter; }
	double getEggCost() { return this->eggCost; }

private:

	// Minimum observed incubation period (Pollet et al. 2021)
	constexpr static double START_HATCH_DAYS = 37.0;

	// High number as an upper limit on egg hatching
	constexpr static double HATCH_DAYS_MAX = 60;

    	// Mean energetic contents of a single egg (kJ) from Montevecchi et al. 1983
	constexpr static double EGG_COST = 69.7;

	/*
	Neglect comes with a developmental cost, increases the necessary
	length of incubation.

	Boersma and Wheelwright (1979) fit a line for Fork-tailed Storm-Petrels,
	with a slope of 0.7 for (days neglect) ~ (total incubation period).
	Each day of neglect is thus expected to add (1/0.7)=1.43 days
	to required incubation time.
	*/
	constexpr static double NEGLECT_PENALTY = 1.43;

	/*
	Maximum conseq. neglect before hatch failure 
	(based on maximum values from Boersma and Wheelwright (1979)
	in Fork-tailed Storm-petrels, 
	matching anecdotal value from LT for Leach's Storm-Petrels
	*/
	constexpr static int NEGLECT_MAX = 7;

	bool alive;	         // is egg alive? 
	bool hatched;		 // is egg hatched?

	double eggCost;
    int neglectMax;      // what is the egg cold limit?

	double currDays;	     // egg age (days) 
	double hatchDays; 	     // current total incubation days required until
	double maxHatchDays; 	 // cutoff before hatching fails due to time limit (e.g., breeding window ends)

	int currNegCounter;	      // current consecutive days of neglect
	int totNegCounter;	      // totals days of neglect
	int maxNegCounter;	      // maximum consecutive days of neglect
};