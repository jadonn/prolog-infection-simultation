individual(ID). %atom representing an individual

advanceIllness(Infected) :-
    /*
    Advance the illness status for infected individuals
    based on how many days since they were infected.
    @param List of infected individuals
    @return none
    */
    [Individual|Remainder] = Infected,
    dayInfected(Individual, Day),
    day(Today),
    (Today - Day =:= 7 ->
    retract(health(Individual, incubation)),
    assert(health(Individual, fever)),
    advanceIllness(Remainder);
    Today - Day =:= 11 ->
    retract(health(Individual, fever)),
    assert(health(Individual, rash)),
    advanceIllness(Remainder);
    Today - Day =:= 14 ->
    retract(health(Individual, rash)),
    assert(health(Individual, pustules)),
    advanceIllness(Remainder);
    Today - Day =:= 18 ->
    random(Survival),
    (Survival >= 0.30 ->
    retract(health(Individual, pustules)),
    assert(health(Individual, normal));
    Survival < 0.30 ->
    retract(health(Individual, pustules)),
    assert(health(Individual, deceased))
    ),
    advanceIllness(Remainder);
    advanceIllness(Remainder)
    ).
    
advanceIllness([]) :- !.    

processContamination(Matches) :-
    /*
    Determine if an individual is contaminated or not
    @param List of uncontaminated individuals
    @return none
    */
    
    [Individual|Remainder] = Matches,
    random(Contaminated),
    (Contaminated >= 0.80 ->
    retract(contaminated(Individual, false)),
    assert(contaminated(Individual, true)),
    processContamination(Remainder);
    Contaminated < 0.80 ->
    processContamination(Remainder)
    ).
    
processContamination([]) :- !.

generateGroupResistance() :-
    /*
    Calculate the resistance scores for groups
    @param none
    @return none
    */
    
    random_between(1, 16, ResistanceMultiplier),
    GroupResistanceFactor is (ResistanceMultiplier * 0.0625 * 0.05),
    assert(groupResistance(GroupResistanceFactor)).

    
startAttack() :-
    /*
    Simulate the start of the attack. Select a random non-family 
    mixing group and process the individuals in that group for 
    infection and contamination.
    @param none
    @return none
    */
    
    random_between(1, 2, NeighborhoodOrGroup),
    (NeighborhoodOrGroup =:= 1 ->
    random_between(0, 3, Neighborhood),
    findall(Individual, individualNeighborhood(Individual, Neighborhood), NMatches),
    processInfection(NMatches);
    NeighborhoodOrGroup =:= 2 ->
    numberOfGroups(NumGroups),
    random_between(1, NumGroups, GroupID),
    findall(Individual, groupMember(Individual, [GroupID, SecondGroup]),  FirstMatches),
    findall(Individual, groupMember(Individual, [FirstGroup, GroupID]), SecondMatches),
    append(FirstMatches, SecondMatches, GMatches),
    processInfection(GMatches)
    ).
    
processInfection(AllMatches) :-
    /*
    Determine if an individual has been infected or not. Infected 
    individuals are also contaminated individuals.
    @param List of uninfected, vulnerable individuals
    @return none
    */
    
    [Individual|Remainder] = AllMatches,
    random(Infected),
    (Infected >= 0.80 ->
    retract(infected(Individual, false)),
    assert(infected(Individual, true)),
    retract(health(Individual, normal)),
    assert(health(Individual, incubation)),
    contaminated(Individual, Contaminated),
    (Contaminated == false ->
    retract(contaminated(Individual, false)),
    assert(contaminated(Individual, true));
    Contaminated == true ->
    !
    ),
    day(Num),
    assert(dayInfected(Individual, Num)),
    processInfection(Remainder);
    Infected < 0.80 ->
    processInfection(Remainder)
    ).
    
processInfection([]) :- !.
    
assignGroups(individual(ID), NumGroups) :-
    /*
    Randomly assign an individual to two non-family, 
    non-neighborhood mixing groups.
    @param individual, number of total groups
    @return none
    */
    
    repeat,
    random_between(1, NumGroups, FirstGroup),
    random_between(1, NumGroups, SecondGroup),
    FirstGroup =\= SecondGroup,
    assert(groupMember(individual(ID), [FirstGroup, SecondGroup])),
    !.
    
assignFamilies(FamilyID, individual(ID)) :-
    /*
    Assign an individual to an existing, non-full family.
    @param int FamilyID, individual
    @return none
    */
    
    numFamilies(Total),
    (FamilyID =< Total ->
    familyMembers(FamilyID, Members),
    familySize(FamilyID, Size),
    length(Members, NumMembers),
    (NumMembers < Size ->
    append(Members, [individual(ID)], NewMembers),
    retract(familyMembers(FamilyID, Members)),
    assert(familyMembers(FamilyID, NewMembers)),
    assert(memberOfFamily(individual(ID), FamilyID));
    NumMembers =:= Size ->
    NewFamilyID is FamilyID + 1,
    assignFamilies(NewFamilyID, individual(ID))
    );
    FamilyID > Total ->
    !
    ).
    
generateFamilies(FamilyID, Population) :-
    /*
    Generate a number of families with sizes between 
    1 and 7 members such that there is space in a family 
    group for every member of the population.
    @param int FamilyID, int total population
    @return none
    */
    
    random_between(1, 7, FamilySize),
    RemainingPopulation is Population - FamilySize,
    NextFamily is FamilyID + 1,
    (
    RemainingPopulation > 0 ->
    assert(familySize(FamilyID, FamilySize)),
    assert(familyMembers(FamilyID, [])),
    generateFamilies(NextFamily, RemainingPopulation);
    RemainingPopulation =< 0 ->
    assert(familySize(FamilyID, Population)),
    assert(familyMembers(FamilyID, [])),
    assert(numFamilies(FamilyID)),
    !
    ).
    
assignNeighborhoods(individual(ID)) :-
    /*
    Assign an individual to a neighborhood.
    @param individual
    @return none
    */
    
    memberOfFamily(individual(ID), FamilyID),
    Neighborhood is mod(FamilyID, 4),
    assert(familyNeighborhood(FamilyID, Neighborhood)),
    assert(individualNeighborhood(individual(ID), Neighborhood)).
    
generateIndividuals([H|T]) :-
    /*
    Generate a list of health, non-contaminated, 
    non-infected individuals. Each individual is 
    assigned a family, a neighborhood, and two random 
    mixing groups.
    @param int Population
    @return none
    */
    
    assert(health(individual(H), normal)),
    assert(contaminated(individual(H), false)),
    assert(infected(individual(H), false)),
    assignResistance(individual(H)),
    numberOfGroups(NumGroups),
    assignGroups(individual(H), NumGroups),
    assignFamilies(1, individual(H)),
    assignNeighborhoods(individual(H)),
    generateIndividuals(T).
    
generateIndividuals([]) :- !.

assignResistance(individual(ID)):-
    /*
    Generate a random resistance score for the given individual
    @param individual
    @return none
    */
    
    random(RandomFloat),
    Pi is 3.14,
    E is 2.74,
    SQRT is sqrt(2 * Pi),
    Score is ((1 / (0.15 * SQRT)) * (E ** (-1 * (((RandomFloat - 1.0) ** 2) / (2 * (0.15 ** 2)))))),
    assert(resistance(individual(ID), Score)).

generateTown:-
    /*
    Generate a town with population 2000 individuals
    @param none
    @return none
    */
    
    numlist(1, 2000, IDList),
    random_between(2, 4, NumGroups),
    assert(numberOfGroups(NumGroups)),
    generateGroupResistance(),
    generateFamilies(1, 2000),
    generateIndividuals(IDList).
    
main :-
    /*
    Main program loop.
    */
    done(Done),
    (Done == false ->
    write('\nMenu\n'),
    write('Type in the keyword in [] followed by a period to make a selection.\n'),
    write('\tDo time step [step]\n'),
    write('\tDisplay results for current day [today]\n'),
    write('\tDisplay status for a particular person PID [person]\n'),
    write('\tDisplay status for a particular mixing group MGID [group]\n'),
    read(Option),
    call(Option),
    main;
    Done == true ->
    recordStatus,
    nl,
    write('\tThe CDC has implemented disease control protocols.'),
    nl,
    write('\tThe simulation has terminated.'),
    nl,
    nl,
    open('simulation.txt', append, Stream),
    outputLog(1, Stream),
    numberOfGroups(GroupTotal),
    numlist(1, GroupTotal, GroupIDs),
    outputGroupLog(GroupIDs, Stream),
    numlist(1, 2000, IDList),
    outputIndividualLog(IDList, Stream),
    close(Stream),
    !
    ).

outputLog(DaysLogged, Stream) :-
    /*
    Output a log of the summary of individual statuses 
    for each completed day of the simulation.
    @param int days logged, output stream
    @return none
    */
    
    status(DaysLogged, NumNormal, NumInfected, NumContaminated, TotalSymptomatic, DeceasedTotal),
    write(Stream, 'Day: '),
    write(Stream, DaysLogged),
    write(Stream, '\tNormal: '),
    write(Stream, NumNormal),
    write(Stream, '\tInfected: '),
    write(Stream, NumInfected),
    write(Stream, '\tContaminated: '),
    write(Stream, NumContaminated),
    write(Stream, '\tSymptomatic: '),
    write(Stream, TotalSymptomatic),
    write(Stream, '\tDeceased: '),
    write(Stream, DeceasedTotal),
    nl(Stream),
    day(Today),
    (DaysLogged < Today ->
    NewDaysLogged is DaysLogged + 1,
    outputLog(NewDaysLogged, Stream);
    DaysLogged >= Today ->
    write("Wrote to file records of daily totals"),
    nl,
    !
    ).
    
outputIndividualLog(IDList, Stream) :-
    /*
    Output to file the record for each individual
    @param int list, output stream
    @return none
    */
    
    [ID|Remainder] = IDList,
    health(individual(ID), Health),
    contaminated(individual(ID), ContStatus),
    infected(individual(ID), InfStatus),
    memberOfFamily(individual(ID), FamilyID),
    groupMember(individual(ID), [GroupOne, GroupTwo]),
    write(Stream, "---Individual: "),
    write(Stream, ID),
    write(Stream, "---"),
    nl(Stream),
    write(Stream, "\tHealth: "),
    write(Stream, Health),
    nl(Stream),
    write(Stream, "\tContaminated: "),
    write(Stream, ContStatus),
    nl(Stream),
    write(Stream, "\tInfected: "),
    write(Stream, InfStatus),
    nl(Stream),
    write(Stream, "\tFamily: "),
    write(Stream, FamilyID),
    nl(Stream),
    write(Stream, "\tGroup one: "),
    write(Stream, GroupOne),
    nl(Stream),
    write(Stream, "\tGroup two: "),
    write(Stream, GroupTwo),
    nl(Stream),
    outputIndividualLog(Remainder, Stream).
    
outputIndividualLog([], Stream) :-
    write(Stream, "---End logging of individuals---"),
    nl(Stream),
    write("Wrote to file records for every individual."),
    nl.
    
outputGroupLog(GroupIDs, Stream) :-
    /*
    Write to file the records of each mixing group.
    @param int list of Group IDs, output stream
    @return none
    */

    [GroupID|Remainder] = GroupIDs,
    write(Stream, "Group: "),
    write(Stream, GroupID),
    nl(Stream),
    findall(Individual, groupMember(Individual, [GroupID, SecondGroup]),  TotalFirstMatches),
    findall(Individual, groupMember(Individual, [FirstGroup, GroupID]), TotalSecondMatches),
    append(TotalFirstMatches, TotalSecondMatches, TotalMatches),
    length(TotalMatches, TotalLength),
    write(Stream, "\tNumber of Members: "),
    write(Stream, TotalLength),
    nl(Stream),
    findall(Individual, (groupMember(Individual, [GroupID, SecondGroup]), infected(Individual, true)),  InfFirstMatches),
    findall(Individual, (groupMember(Individual, [FirstGroup, GroupID]), infected(Individual, true)), InfSecondMatches),
    append(InfFirstMatches, InfSecondMatches, InfMatches),
    length(InfMatches, InfMatchesLength),
    write(Stream, "\tInfected Members: "),
    write(Stream, InfMatchesLength),
    nl(Stream),
    findall(Individual, (groupMember(Individual, [GroupID, SecondGroup]), contaminated(Individual, true)),  ContFirstMatches),
    findall(Individual, (groupMember(Individual, [FirstGroup, GroupID]), contaminated(Individual, true)), ContSecondMatches),
    append(ContFirstMatches, ContSecondMatches, ContMatches),
    length(ContMatches, ContMatchesLength),
    write(Stream, "\tContaminated Members: "),
    write(Stream ,ContMatchesLength),
    nl(Stream),
    findall(Individual, (groupMember(Individual, [GroupID, SecondGroup]), health(Individual, fever)), FirstFeverMatches),
    findall(Individual, (groupMember(Individual, [FirstGroup, GroupID]), health(Individual, fever)), SecondFeverMatches),
    append(FirstFeverMatches, SecondFeverMatches, AllFeverMatches),
    findall(Individual, (groupMember(Individual, [GroupID, SecondGroup]), health(Individual, rash)), FirstRashMatches),
    findall(Individual, (groupMember(Individual, [FirstGroup, GroupID]), health(Individual, rash)), SecondRashMatches),
    append(FirstRashMatches, SecondRashMatches, AllRashMatches),
    findall(Individual, (groupMember(Individual, [GroupID, SecondGroup]), health(Individual, pustules)), FirstPustulesMatches),
    findall(Individual, (groupMember(Individual, [FirstGroup, GroupID]), health(Individual, pustules)), SecondPustulesMatches),
    append(FirstPustulesMatches, SecondPustulesMatches, AllPustulesMatches),
    append(AllFeverMatches, AllRashMatches, AllFirstMatches),
    append(AllFirstMatches, AllPustulesMatches, SymptomaticMatches),
    length(SymptomaticMatches, TotalSymptomatic),
    write(Stream, "\tSymptomatic members: "),
    write(Stream, TotalSymptomatic),
    nl(Stream),
    findall(Individual, (groupMember(Individual, [GroupID, SecondGroup]), health(Individual, deceased)), FirstDeceasedMatches),
    findall(Individual, (groupMember(Individual, [FirstGroup, GroupID]), health(Individual, deceased)), SecondDeceasedMatches),
    append(FirstDeceasedMatches, SecondDeceasedMatches, AllDeceasedMatches),
    length(AllDeceasedMatches, DeceasedTotal),
    write(Stream, "\tDeceased: "),
    write(Stream, DeceasedTotal),
    nl(Stream),
    outputGroupLog(Remainder, Stream).
    
outputGroupLog([], Stream):-
    write(Stream, "---End of Group logs---"),
    nl(Stream),
    write("Wrote to file records for every group."),
    nl.
    
step :- 
    /*
    Advance the simulation by one day. Also determine 
    if the CDC has been alerted to the infection.
    @param none
    @return none
    */
    
    recordStatus,
    retract(day(Yesterday)),
    Today is Yesterday + 1,
    assert(day(Today)),
    findall(Individual, infected(Individual, true), Infected),
    advanceIllness(Infected),
    findall(Individual, infected(Individual, false), NonInfected),
    processInfection(NonInfected),
    findall(Individual, contaminated(Individual, false), NonContaminated),
    processContamination(NonContaminated),
    findall(Individual, health(Individual, pustules), PustulesMatches),
    length(PustulesMatches, CasesOfPustules),
    lastDay(End),
    (End =:= 0 ->
        (CasesOfPustules > 0 ->
            random(AlertCDC),
            (AlertCDC >= 0.475 ->
                FinalDay is Today + 2,
                retract(lastDay(End)),
                assert(lastDay(FinalDay));
            AlertCDC < 0.475 ->
                !
            );
        CasesOfPustules =:= 0 ->
            !
        );
    End =:= Today ->
        retract(done(false)),
        assert(done(true));
        !
    ),
    today.

recordStatus :-
    /*
    Record a snapshot of the simulation status for the current day
    @param none
    @return none
    */
    
    day(Today),
    findall(Individual, health(Individual, normal), NormalHealthList),
    length(NormalHealthList, NumNormal),
    findall(Individual, infected(Individual, true), InfectedList),
    length(InfectedList, NumInfected),
    findall(Individual, contaminated(Individual, true), ContaminatedList),
    length(ContaminatedList, NumContaminated),
    findall(Individual, health(Individual, fever), FeverMatches),
    findall(Individual, health(Individual, rash), RashMatches),
    findall(Individual, health(Individual, pustules), PustulesMatches),
    append(FeverMatches, RashMatches, FirstMatches),
    append(FirstMatches, PustulesMatches, SymptomaticMatches),
    length(SymptomaticMatches, TotalSymptomatic),
    findall(Individual, health(Individual, deceased), DeceasedMatches),
    length(DeceasedMatches, DeceasedTotal),
    assert(status(Today, NumNormal, NumInfected, NumContaminated, TotalSymptomatic, DeceasedTotal)).
    
today :- 
    /*
    Output a summary of the simulations status for the current day to STDOUT
    @param none
    @return none
    */
    
    day(Today),
    write(Today),
    write(" day(s) have passed since infection started."),
    nl,
    findall(Individual, health(Individual, normal), NormalHealthList),
    length(NormalHealthList, NumNormal),
    write("Normal: "),
    write(NumNormal),
    nl,
    findall(Individual, infected(Individual, true), InfectedList),
    length(InfectedList, NumInfected),
    write("Infected: "),
    write(NumInfected),
    nl,
    findall(Individual, contaminated(Individual, true), ContaminatedList),
    length(ContaminatedList, NumContaminated),
    write("Contaminated: "),
    write(NumContaminated),
    nl,
    write("Symptomatic: "),
    findall(Individual, health(Individual, fever), FeverMatches),
    findall(Individual, health(Individual, rash), RashMatches),
    findall(Individual, health(Individual, pustules), PustulesMatches),
    append(FeverMatches, RashMatches, FirstMatches),
    append(FirstMatches, PustulesMatches, SymptomaticMatches),
    length(SymptomaticMatches, TotalSymptomatic),
    write(TotalSymptomatic),
    nl,
    write("Deceased: "),
    findall(Individual, health(Individual, deceased), DeceasedMatches),
    length(DeceasedMatches, DeceasedTotal),
    write(DeceasedTotal),
    nl.
    
person :- 
    /*
    Wrapper for reportPerson for getting the 
    ID that should be reported on.
    @param none
    @return none
    */
    write("Enter search ID\n"),
    read(SearchID),
    reportPerson(SearchID).

reportPerson(SearchID) :-
    /*
    Output to STDOUT all of the information the database 
    has for a given person relevant to the 
    simulation progression.
    @param int SearchID
    @return none
    */
    nl,
    write("\t---Report on Person "),
    write(SearchID),
    write("---"),
    nl,
    write("Status: "),
    health(individual(SearchID), Status),
    write(Status),
    write("\t\tInfected? "),
    infected(individual(SearchID), InfectedStatus),
    write(InfectedStatus),
    write("\t\tContaminated? "),
    contaminated(individual(SearchID), ContaminatedStatus),
    write(ContaminatedStatus),
    nl,
    write("Family: "),
    memberOfFamily(individual(SearchID), FamilyID),
    write(FamilyID),
    write("\t\tNeighborhood: "),
    individualNeighborhood(individual(SearchID), NeighborhoodID),
    write(NeighborhoodID),
    write("\t\tMember of Groups: "),
    groupMember(individual(SearchID), Group),
    write(Group),
    nl,
    (InfectedStatus == true ->
    write("Infected on day: "),
    dayInfected(individual(SearchID), Day),
    write(Day);
    InfectedStatus == false ->
    !
    ).

group :- write("Enter group ID\n"),
    /*
    Wrapper for prompting for the group ID to be 
    passed to the reportGroup predicate
    @param none
    @return none
    */
    
    read(GroupID),
    reportGroup(GroupID).
    
reportGroup(GroupID) :-
    /*
    Outputs the status of members of the particular group to STDOUT
    @param int group ID
    @return none
    */
    
    write("Group: "),
    write(GroupID),
    nl,
    findall(Individual, groupMember(Individual, [GroupID, SecondGroup]),  TotalFirstMatches),
    findall(Individual, groupMember(Individual, [FirstGroup, GroupID]), TotalSecondMatches),
    append(TotalFirstMatches, TotalSecondMatches, TotalMatches),
    length(TotalMatches, TotalLength),
    write("\tNumber of Members: "),
    write(TotalLength),
    nl,
    findall(Individual, (groupMember(Individual, [GroupID, SecondGroup]), infected(Individual, true)),  InfFirstMatches),
    findall(Individual, (groupMember(Individual, [FirstGroup, GroupID]), infected(Individual, true)), InfSecondMatches),
    append(InfFirstMatches, InfSecondMatches, InfMatches),
    length(InfMatches, InfMatchesLength),
    write("\tInfected Members: "),
    write(InfMatchesLength),
    nl,
    findall(Individual, (groupMember(Individual, [GroupID, SecondGroup]), contaminated(Individual, true)),  ContFirstMatches),
    findall(Individual, (groupMember(Individual, [FirstGroup, GroupID]), contaminated(Individual, true)), ContSecondMatches),
    append(ContFirstMatches, ContSecondMatches, ContMatches),
    length(ContMatches, ContMatchesLength),
    write("\tContaminated Members: "),
    write(ContMatchesLength),
    nl,
    findall(Individual, (groupMember(Individual, [GroupID, SecondGroup]), health(Individual, fever)), FirstFeverMatches),
    findall(Individual, (groupMember(Individual, [FirstGroup, GroupID]), health(Individual, fever)), SecondFeverMatches),
    append(FirstFeverMatches, SecondFeverMatches, AllFeverMatches),
    findall(Individual, (groupMember(Individual, [GroupID, SecondGroup]), health(Individual, rash)), FirstRashMatches),
    findall(Individual, (groupMember(Individual, [FirstGroup, GroupID]), health(Individual, rash)), SecondRashMatches),
    append(FirstRashMatches, SecondRashMatches, AllRashMatches),
    findall(Individual, (groupMember(Individual, [GroupID, SecondGroup]), health(Individual, pustules)), FirstPustulesMatches),
    findall(Individual, (groupMember(Individual, [FirstGroup, GroupID]), health(Individual, pustules)), SecondPustulesMatches),
    append(FirstPustulesMatches, SecondPustulesMatches, AllPustulesMatches),
    append(AllFeverMatches, AllRashMatches, AllFirstMatches),
    append(AllFirstMatches, AllPustulesMatches, SymptomaticMatches),
    length(SymptomaticMatches, TotalSymptomatic),
    write("\tSymptomatic members: "),
    write(TotalSymptomatic),
    nl,
    findall(Individual, (groupMember(Individual, [GroupID, SecondGroup]), health(Individual, deceased)), FirstDeceasedMatches),
    findall(Individual, (groupMember(Individual, [FirstGroup, GroupID]), health(Individual, deceased)), SecondDeceasedMatches),
    append(FirstDeceasedMatches, SecondDeceasedMatches, AllDeceasedMatches),
    length(AllDeceasedMatches, DeceasedTotal),
    write("\tDeceased: "),
    write(DeceasedTotal),
    nl.
    
go :-
    /*
    Main entry point for the program and 
    the main program loop. This predicate 
    also cleans up the environment for 
    new run of the simulation. This 
    predicate is also responsible for 
    setting up the simulation.
    */
    retractall(health(_,_)),
    retractall(groupMember(_,_)),
    retractall(contaminated(_,_)),
    retractall(infected(_,_)),
    retractall(dayInfected(_,_)),
    retractall(day(_)),
    retractall(numFamilies(_)),
    retractall(familySize(_,_)),
    retractall(familyMembers(_,_)),
    retractall(memberOfFamily(_,_)),
    retractall(individualNeighborhood(_,_)),
    retractall(familyNeighborhood(_,_)),
    retractall(numberOfGroups(_)),
    retractall(groupResistance(_)),
    retractall(resistance(_,_)),
    retractall(done(_)),
    retractall(lastDay(_)),
    assert(lastDay(0)),
    assert(day(1)),
    assert(done(false)),
    generateTown,
    startAttack,
    main.
    