namespace Logic

open System
open TimeOff

[<CLIMutable>]
type TimeOffBalance = {
    Username: string;
    AllotmentAccruedToDate: double;
    CarriedOverFromLastYear: double;
    Taken: double;
    Planned: double;
    CurrentBalance: double;
}



module TimeOffCalc = 
    // A
    let CumulLastYear userRequests currentDate (user: User)= 
        0
     
    //B
    let ReportLastYar userRequests currentDate (user: User)= 
        0
       
    //C 
    let timeOffEffectif userRequests currentDate (user: User)= 
        0
       
    //D 
    let timeOffPrevus userRequests currentDate (user: User)= 
        0
        
     //E => A + B - (C + D)
    let AvailableTimeOff userRequests currentDate (user: User)=
        0 

    let summary userRequests currentDate (user: User)=
        let result = {
            CurrentBalance= 0.;
            Username= "";
            AllotmentAccruedToDate=0.;
            CarriedOverFromLastYear=0.;
            Taken=0.;
            Planned=0.;
        }
        result

