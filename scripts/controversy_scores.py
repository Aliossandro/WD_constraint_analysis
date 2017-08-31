import pandas as pd
import calendar
import datetime

file_name = './edited_for_conflict.csv'

#Load file
ew_set = pd.read_csv(file_name, sep = '\t', low_memory=False)

ew_set['time_stamp'] = pd.to_datetime(ew_set['time_stamp'], format="%Y-%m-%d %H:%M:%S")

ew_set['year_month'] = [date.strftime("%b %Y") for date in ew_set['time_stamp']]


ew_grouped = ew_set.groupby('stat_id')

# ew_set = ew_set.set_index(['year_month'])
# ew_set.rolling('month').count()

#Generate disagreement scores per item
disagreement_scores = {}
for name, group in ew_grouped:
    # print group
    # print group.iloc[0]['user_name']
    dis_counter = 0
    # disagreement_score = {}
    for i in xrange(1, group.shape[0]):
        if i > 1:
            if group.iloc[i]['disagreement'] == True:

                if group.iloc[i-1]['user_name'] == group.iloc[i]['user_name']:
                    # print 'yes'
                    dis_counter += 1

    disagreement_scores[name] = float(dis_counter) / group.shape[0]


d = dict((k, v) for k, v in disagreement_scores.items() if v > 0)

d = disagreement_scores.items()

dis_panda = pd.DataFrame(d)
dis_panda.columns = ['stat_id', 'dis_score']

# dis_panda.to_csv('dis_panda.csv', index=False)


#
# ew_set = ew_set.set_index(['year_month'])
# ew_set.rolling('month').count()



#disagreement score per month/property
ew_grouped = ew_set.groupby('stat_property')


disagreement_scores_list = []
for name, group in ew_grouped:

    month_group = group.groupby('year_month')
    dis_counter = 0

    for stat, month in month_group:
        dis_month = 0
        # print month.iloc[0]['stat_property']

        # print group
        # print group.iloc[0]['user_name']

        disagreement_scores = {}
        for i in xrange(1,month.shape[0]):
            if i > 1:
                if month.iloc[i]['disagreement'] == True:

                    if month.iloc[i-1]['user_name'] != month.iloc[i]['user_name']:
                        # print 'yes'
                        dis_month += 1


        dis_counter += dis_month
        # print dis_counter / group.shape[0]
        disagreement_scores['stat_id'] = name
        disagreement_scores['dis_score'] = float(dis_month) / month.shape[0]
        disagreement_scores['dis_score_roll'] = float(dis_counter) / month.shape[0]
        disagreement_scores['month'] = stat
        disagreement_scores['property_stat'] = month.iloc[0]['stat_property']
        dis_month = 0
        disagreement_scores_list.append(disagreement_scores)



dis_panda = pd.DataFrame(disagreement_scores_list)


# dis_panda.to_csv('dis_panda_month.csv', index=False)
