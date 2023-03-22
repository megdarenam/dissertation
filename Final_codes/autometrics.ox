#include <oxstd.oxh>
#import <packages/PcGive/pcgive>

static decl mPrev, repetitions=103, horizonte=12;



main()
{
// This program requires a licenced version of PcGive Professional.
	//--- Ox code for EQ( 2)

	decl i;
	
	mPrev = nans(horizonte,repetitions+1);
	
	for(i = 0 ; i <= repetitions ; i++)
	{
	
	decl model = new PcGive();

	model.Load("C:\\Users\\megda\\OneDrive\\Dissertação\\data_tratada.csv");
	model.Deterministic(-1);

	// Formulation of the GUM (commented out)

	model.DeSelect();
	model.Select("Y", {"desas_prod", 0, 0});
	model.Select("X", {"Constant", 0, 0});
	model.Select("Y", {"desas_prod", 1, 13});
	model.Autometrics(0.01, "none", 1);

	model.SetSelSample(1, 1, 120 + i, 1);
	model.SetMethod("OLS");
	model.Estimate();
	
	decl aPrev=model.Forecast(horizonte);

	mPrev[0:][i] = aPrev[0];				

	delete model;

	

	}
	
	println(mPrev);
	savemat("previsoes_autometrix_rolling_window_120.xlsx" , mPrev);
	
}
