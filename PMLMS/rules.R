#Conformance Checking

pacman::p_load(bupaR, processcheckR)

sepsis %>% check_rule(starts('ER Registration'), label='r1')

events2 %>% check_rule(starts('PPT'),label='r1') %>% check_rule(and('Assignment','Quiz'),label='r2') %>% group_by(r1, r2) %>% n_cases()

events2 %>% check_rules(r1= starts('PPT'), r2=ends('Quiz')) %>% group_by(r1, r2) %>% n_cases()

events2 %>% check_rules(r1= starts('PPT'), r2=ends('Assignment')) %>% group_by(r1, r2) %>% n_cases()

events2 %>% activity_labels()
events2 %>% filter_rules(r1= starts('PPT'), r2=and('Quiz','Assignment')) %>% n_cases()

events2 %>% process_map()
events2 %>% filter_rules(r1= starts('Survey')) %>% n_events()
events2 %>% filter_rules(r1= starts('Survey'), r2=and('URL','Quiz')) %>% n_events()

sepsis %>% filter_rules(r1= starts('ER Registration')) %>% n_cases()

sepsis %>% n_cases()
sepsis %>% check_rules(contains('Leucocytes', n=100)) %>% n_cases()

events2 %>% check_rules(contains('Quiz', n > 5)) %>% group_by(user) %>% n_cases() 
?check_rule
events2 %>% check_rule(precedence("PPT","Quiz")) %>% group_by(precedence_PPT_Quiz) %>% n_cases()
events2 %>% check_rule(contains("PPT",n=3), label='PPT_3') %>% group_by(PPT_3) %>% n_cases()

events2 %>% check_rule(contains_between(activity="PPT",min=2, max=5), label='PPT_2_5') %>% group_by(PPT_2_5) %>% n_cases()

events2 %>% check_rule(contains_between(activity=c("PPT"),min=2, max=5), label='PPT_2_5') %>% group_by(PPT_2_5) %>% n_cases()

events2 %>% check_rule(absent(activity=c("PPT"),n=0), label='PPT_0') %>% group_by(PPT_0) %>% n_cases() #no PPT

events2 %>% check_rule(absent(activity=c("PPT"),n=10), label='PPT_10') %>% group_by(PPT_10) %>% n_cases()  #PPT 0 to 18 times

#2 at a time------
events2 %>% check_rule(succession("PPT",'Assignment'), label='r1') %>% group_by(r1) %>% n_cases()  #sequence
events2 %>% filter_rules(r1 = starts("Forum")) %>% filter_rules(r2 = ends('Assignment')) %>% group_by(r1,r2) %>% n_cases()  #sequence

events2 %>% check_rule(starts('PPT'),label='r1') %>% check_rule(ends('Assignment'),label='r2') %>% group_by(r1, r2) %>% n_cases()

events2 %>% check_rule(succession('PPT','Assignment'),label='r1') %>% check_rule(precedence('Quiz','Assignment'),label='r2') %>% group_by(r1, r2) %>% n_cases()

events2 %>% check_rule(succession('PPT','Assignment'),label='r1') %>% check_rule(precedence('Quiz','Assignment'),label='r2') %>% filter(r1==T & r2==T) %>% case_labels()

events2 %>% resource_map()
events2 %>% resource_matrix()
events2 %>% resource_matrix() %>% plot()



detect_activity_frequency_violations(activitylog = events2, 'PPT'=1)
