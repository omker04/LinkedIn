import json
import numpy as np
from pprint import pprint
import pandas as pd
from igraph import *


def read_data() :
    with open('data/person_json.json') as json_data:
        global person
        person = json.load(json_data)
        json_data.close()
        
    with open('data/skill_json.json') as json_data:
        global skill
        skill = json.load(json_data)
        json_data.close()
        
    with open('data/skill_person_json.json') as json_data:
        global skill_person
        skill_person = json.load(json_data)
        json_data.close()
        
    with open('data/endorsement_json.json') as json_data:
        global endorsement
        endorsement = json.load(json_data)
        json_data.close()
    
    with open('data/connection_json.json') as json_data:
        global connection
        connection = json.load(json_data)
        json_data.close()
        
    with open('data/endorsement_rating_json.json') as json_data:
        global endorsement_rating
        endorsement_rating = json.load(json_data)
        json_data.close()
     
    with open('data/endorsement_refresh_table_json.json') as json_data:
        global endorsement_refresh_table
        endorsement_refresh_table = json.load(json_data)
        json_data.close()
    
    with open('data/manager_json.json') as json_data:
        global manager
        manager = json.load(json_data)
        json_data.close()
        
    with open('data/profile_json.json') as json_data:
        global profile
        profile = json.load(json_data)
        json_data.close()
        
    global connection_graph
    connection_graph = Graph.Read_Pickle('data/connection_graph.pickle')
    return 'all data read successfully'

def write_data() :
    with open('data/person_json.json', 'w') as json_data:  
        json.dump(person, json_data)
        json_data.close()
    
    with open('data/skill_json.json', 'w') as json_data:
        json.dump(skill, json_data)
        json_data.close()
        
    with open('data/skill_person_json.json', 'w') as json_data:
        json.dump(skill_person, json_data)
        json_data.close()
        
    with open('data/endorsement_json.json', 'w') as json_data:
        json.dump(endorsement, json_data)
        json_data.close()
    
    with open('data/connection_json.json', 'w') as json_data:
        json.dump(connection, json_data)
        json_data.close()
        
    with open('data/endorsement_rating_json.json', 'w') as json_data:
        json.dump(endorsement_rating, json_data)
        json_data.close()
     
    with open('data/endorsement_refresh_table_json.json', 'w') as json_data:
        json.dump(endorsement_refresh_table, json_data)
        json_data.close()
    
    with open('data/manager_json.json', 'w') as json_data:
        json.dump(manager, json_data)
        json_data.close()
        
    with open('data/profile_json.json', 'w') as json_data:
        json.dump(profile, json_data)
        json_data.close()
    
    connection_graph.write_pickle('data/connection_graph.pickle')
    return 'all data saved successfully'

def add_new_user(GTS_id, Name) :
    read_data()
    if GTS_id in skill_person.keys() :
        if Name == person[GTS_id]['Name'] :
            out_string = '%s is already signed up.' % GTS_id
        else :
            out_string = 'GTS_ID and Name doesnt match.'
    else :
        skill_person[GTS_id] = {'rate':[], 'skills':[]}
        endorsement[GTS_id] = {}
        connection[GTS_id] = None
        person[GTS_id] = {'GTS_ID' : GTS_id, 'Name' : Name, 'Manager' : None}
        connection_graph.add_vertices([GTS_id])
        out_string = '%s just signed up.' % GTS_id
        profile[GTS_id] = {}
        profile[GTS_id]['Name'] = Name
        profile[GTS_id]['ConnectionInitiated'] = []
        profile[GTS_id]['ConnectionReceived'] = []
        profile[GTS_id]['Manager'] = None
        profile[GTS_id]['Connections'] = 0
        profile[GTS_id]['Skills'] = []
        profile[GTS_id]['Endorsements'] = {'Received' : 0,
                                      'Endorsed' : 0,
                                      'New' : []
                                     }
        profile[GTS_id]['DirectReports'] = None
        endorsement_refresh_table[GTS_id] = {}
    write_data()
    return out_string

def remove_user(GTS_id) :
    read_data()
    if GTS_id in skill_person.keys() :
        skill_person.pop(GTS_id, None)
        id_presence = [GTS_id + '_' in k for k in endorsement_rating.keys()]
        endorsed = list(np.array(endorsement_rating.keys())[id_presence])
        for k in endorsed :
            endorsement_rating.pop(k, None)
        endorsement.pop(GTS_id, None)
        connected_to = connection[GTS_id]
        connection.pop(GTS_id, None)
        if connected_to != None :
            for k in connected_to :
                connection[k].remove(GTS_id)
        manager_ID = person[GTS_id]['Manager']
        person.pop(GTS_id, None)
        if manager_ID != None:
            manager[manager_ID].remove(GTS_id)
        out_string = '%s has been successfully deleted from database.' % GTS_id
        connection_graph.delete_vertices([connection_graph.vs.find(name = GTS_id).index])
        profile.pop(GTS_id)
        endorsement_refresh_table.pop(GTS_id)
    else :
        out_string = '%s is not in our database.' % GTS_id
    write_data()
    return out_string

def change_manager(GTS_id, new_manager_id) :
    read_data()
    old_manager = person[GTS_id]['Manager']
    all_managers = [person[k]['Name'] for k in manager.keys()]
    if old_manager == None or old_manager not in all_managers :
        manager[new_manager_id].append(GTS_id)
        person[GTS_id]['Manager'] = new_manager_id
        profile[GTS_id]['Manager'] = person[new_manager_id]['Name']
        profile[new_manager_id]['DirectReports'].append(person[GTS_id]['Name'])
        write_data()
        initiate_connection(GTS_id, new_manager_id)
        add_connection(GTS_id, new_manager_id)
        out_string = 'Manager of %s updated' %(GTS_id)
    elif old_manager == new_manager_id :
        out_string = 'Manager of %s is already updated' %(GTS_id)
    else :
        manager[old_manager].remove(GTS_id)
        manager[new_manager_id].append(GTS_id)
        person[GTS_id]['Manager'] = new_manager_id
        write_data()
        initiate_connection(GTS_id, new_manager_id)
        add_connection(GTS_id, new_manager_id)
        profile[new_manager_id]['DirectReports'].append(person[GTS_id]['Name'])
        profile[old_manager]['DirectReports'].remove(person[GTS_id]['Name'])
        profile[GTS_id]['Manager'] = person[new_manager_id]['Name']        
        out_string = 'Manager of %s successfully changed from %s to %s' %(GTS_id, old_manager, new_manager_id)
    write_data()
    return out_string


def get_all_skills(GTS_id) :
    read_data()
    all_skills = [skill['Skill'][k-1] for k in skill_person[GTS_id]['skills']]
    return all_skills

def add_new_skill(new_skill) :
    read_data()
    if new_skill not in skill['Skill'] :
        max_id = max(skill['Skill_ID'])
        skill['Skill'].append(new_skill)
        skill['Skill_ID'].append(max_id + 1)
        out_string = '%s skill added to the skill list.' % new_skill
    else :
        out_string = '%s skill is already listed in our database.' % new_skill
    write_data()
    return out_string

def add_skill_to_person(GTS_id, skill_to_add) :
    read_data()
    if skill_to_add in skill['Skill'] :
        if (skill['Skill'].index(skill_to_add) + 1) not in skill_person[GTS_id]['skills'] :
            skill_person[GTS_id]['skills'].append(skill['Skill'].index(skill_to_add) + 1)
            endorsement[GTS_id][skill['Skill'].index(skill_to_add) + 1] = None
            skill_person[GTS_id]['rate'].append(1)
            out_string = '%s has added %s as a skill.' % (GTS_id, skill_to_add)
            profile[GTS_id]['Skills'].append(skill_to_add)
            endorsement_refresh_table[GTS_id][str(skill['Skill'].index(skill_to_add) + 1)] = {'endorsed' : 0, 'received' : 0, 'skilled_connection' : 0}
        else :
            out_string = '%s is already skilled in %s.' % (GTS_id, skill_to_add)
    else :
        print add_new_skill(skill_to_add)
        skill_person[GTS_id]['skills'].append(skill['Skill'].index(skill_to_add) + 1)
        endorsement[GTS_id][skill['Skill'].index(skill_to_add) + 1] = None
        skill_person[GTS_id]['rate'].append(1)
        profile[GTS_id]['Skills'].append(skill_to_add)
        endorsement_refresh_table[GTS_id][str(skill['Skill'].index(skill_to_add) + 1)] = {'endorsed' : 0, 'received' : 0, 'skilled_connection' : 0}
        out_string = '%s has added %s skill.' % (GTS_id, skill_to_add) 
    write_data()
    return out_string


def get_persons_with_skill(skill_ID) :
    read_data()
    skilled_person = []
    skill_rank = []
    for k in skill_person.keys() :
        if skill_ID in skill_person[k]['skills'] :
            skilled_person.append(k)
            skill_index = skill_person[k]['skills'].index(skill_ID)
            skill_rank.append(skill_person[k]['rate'][skill_index])
    ordered_person = [x for _,x in sorted(zip(skill_rank, skilled_person), reverse = True)]
    return ordered_person

def skill_search(skill_search_string) :
    read_data()
    skill_match = [skill_search_string.lower() in k for k in [j.lower() for j in skill['Skill']]]
    matched_skill = list(np.array(skill['Skill_ID'])[skill_match])
    output = {}
    for k in matched_skill :
        output[skill['Skill'][k-1]] = get_persons_with_skill(k)
    return output


def initiate_connection(from_GTS_id, to_GTS_id) :
    read_data()
    if (from_GTS_id not in connection[to_GTS_id] and to_GTS_id not in connection[from_GTS_id] and from_GTS_id not in profile[to_GTS_id]['ConnectionReceived'] and to_GTS_id not in profile[from_GTS_id]['ConnectionInitiated']) :
        profile[from_GTS_id]['ConnectionInitiated'].append(to_GTS_id)
        profile[to_GTS_id]['ConnectionReceived'].append(from_GTS_id)
        out_string = '%s has initiated connection with %s' % (from_GTS_id, to_GTS_id)
    else :
        out_string = '%s has failed to initiate connection with %s' % (from_GTS_id, to_GTS_id)
    write_data()
    return out_string


def add_connection(from_GTS_id, to_GTS_id) :
    read_data()
    if from_GTS_id == to_GTS_id :
        out_string = '%s cannot connect with self' % (from_GTS_id)
    elif from_GTS_id in connection.keys() and to_GTS_id in connection.keys() :
        if connection[from_GTS_id] == None :
            connection[from_GTS_id] = [to_GTS_id]
            out_string = '%s and %s are now connected.' %(from_GTS_id, to_GTS_id)
        elif to_GTS_id in connection[from_GTS_id] :
            out_string = '%s and %s are already connected.' %(from_GTS_id, to_GTS_id)
        else :
            connection[from_GTS_id].append(to_GTS_id)
            out_string = '%s and %s are now connected.' %(from_GTS_id, to_GTS_id)
        
        if connection[to_GTS_id] == None :
            connection[to_GTS_id] = [from_GTS_id]
            out_string = '%s and %s are now connected.' %(from_GTS_id, to_GTS_id)
        elif 'already' in out_string :
            out_string = '%s and %s are already connected.' %(from_GTS_id, to_GTS_id)
        else :
            connection[to_GTS_id].append(from_GTS_id)
            out_string = '%s and %s are now connected.' %(from_GTS_id, to_GTS_id)
    else :
        if from_GTS_id not in connection.keys() and to_GTS_id not in connection.keys() :
            out_string = 'Both %s and %s has not yet been signed up in our database. Couldnt add the connection.' % (from_GTS_id, to_GTS_id)
        elif from_GTS_id not in connection.keys() and to_GTS_id in connection.keys() :
            out_string = '%s has not yet been signed up in our database. Couldnt add the connection.' % from_GTS_id
        else :
            out_string = '%s has not yet been signed up in our database. Couldnt add the connection.' % to_GTS_id
    if 'now connected'in out_string :
        connection_graph.add_edges([(from_GTS_id, to_GTS_id)])
        connection_graph.es[connection_graph.get_eid(from_GTS_id, to_GTS_id)]['weight'] = 999
        profile[from_GTS_id]['ConnectionInitiated'].remove(to_GTS_id)
        profile[to_GTS_id]['ConnectionReceived'].remove(from_GTS_id)
        profile[from_GTS_id]['Connections'] = profile[from_GTS_id]['Connections'] + 1
        profile[to_GTS_id]['Connections'] = profile[to_GTS_id]['Connections'] + 1
    write_data()
    return out_string

def add_connections_under_manager(GTS_id) :
    read_data()
    if GTS_id in manager.keys() and GTS_id != 'GTS9999':
        associates = manager[GTS_id]
        associates.append(GTS_id)
        for i in range(len(associates)) :
            for j in range((i + 1), len(associates)) :
                initiate_connection(associates[i], associates[j])
                add_connection(associates[i], associates[j])
        out_string = 'All connections under manager %s established.' % person[GTS_id]['Name']
    else :
        out_string = '%s is not a manager.' % GTS_id
    return out_string

# for k in manager.keys() :
#     if k != '' :
#         print add_connections_under_manager(k)
def get_persons_with_skill_ordered(skill_ID) :
    read_data()
    skilled_people = get_persons_with_skill(skill_ID)
    skill_rate = []
    for k in skilled_people :
        skill_rate.append(get_aggregate_skill_rank(k, skill_ID))
    ordered_person = [x for _,x in sorted(zip(skill_rate, skilled_people), reverse = True)]
    return ordered_person

def add_endorsement(to_GTS_id, skill_ID, from_GTS_id) :
    read_data()
    str_skill_ID = str(skill_ID)
    if from_GTS_id == to_GTS_id :
        print 'same'
    else :
        print 'different'
    a = {'endorsement ID' : to_GTS_id + '_' + str(skill_ID) + '_' + from_GTS_id, 'endorser' : from_GTS_id}
    endorsement[to_GTS_id]['165'].append(2)
    endorsement_ID = to_GTS_id + '_' + str(skill_ID) + '_' + from_GTS_id
    endorsement_rating[endorsement_ID] = 1
    p1 = endorsement_refresh_table[from_GTS_id][str(skill_ID)]['endorsed']
    p2 = endorsement_refresh_table[to_GTS_id][str(skill_ID)]['endorsed']
    endorsement_refresh_table[from_GTS_id][str(skill_ID)]['endorsed'] = p1 + 1.0
    endorsement_refresh_table[to_GTS_id][str(skill_ID)]['received'] = p2 + 1.0
    write_data()
    read_data()
    b = len([k for k in connection[from_GTS_id] if k in get_persons_with_skill(skill_ID)])
    endorsement_refresh_table[from_GTS_id][str(skill_ID)]['skilled_connection'] = b
    n = p1 + 1.0
    c = len([k for k in connection[from_GTS_id] if k in get_persons_with_skill(skill_ID)])
    p3 = skill_person[from_GTS_id]['rate'][skill_person[from_GTS_id]['skills'].index(skill_ID)]
    skill_person[from_GTS_id]['rate'][skill_person[from_GTS_id]['skills'].index(skill_ID)] = p3 + 1.0/float(n)
    print skill_person[from_GTS_id]['rate'][skill_person[from_GTS_id]['skills'].index(skill_ID)]
    endorsed = [k for k in connection[from_GTS_id] if k + '_' + str(skill_ID) + '_' + from_GTS_id in endorsement_rating.keys()]
    IDs = [k + '_' + str(skill_ID) + '_' + from_GTS_id for k in endorsed]
    for i in IDs :
        endorsement_rating[i] = c / float(len(IDs))
        print endorsement_rating[i]
    edge_id = connection_graph.get_eid(from_GTS_id, to_GTS_id)
    if connection_graph.es[edge_id]['weight'] == 0 :
        connection_graph.es[edge_id]['weight'] = 0
    else :
        connection_graph.es[edge_id]['weight'] -= 1
    profile[from_GTS_id]['Endorsements']['Endorsed'] += 1
    profile[to_GTS_id]['Endorsements']['Received'] += 1
    profile[to_GTS_id]['Endorsements']['New'].append(endorsement_ID)
    write_data()
    return 'endorsement successful'    

# def add_endorsement(to_GTS_id, skill_ID, from_GTS_id) :
#     read_data()
#     # print connection[to_GTS_id]
#     if connection[to_GTS_id] == None or connection[from_GTS_id] == None or from_GTS_id not in connection[to_GTS_id]:
#         out_string = '%s is not connected to %s.' % (from_GTS_id, to_GTS_id)
#     elif str(skill_ID) not in endorsement[to_GTS_id].keys() :
#         out_string = '%s is not skilled in %s. Endorsement not possible.' % (to_GTS_id, skill['Skill'][skill['Skill_ID'].index(skill_ID)])
#     elif str(skill_ID) not in endorsement[from_GTS_id].keys() :
#         out_string = '%s is not skilled in %s. Endorsement not possible.' % (from_GTS_id, skill['Skill'][skill['Skill_ID'].index(skill_ID)])
#     elif endorsement[to_GTS_id][str(skill_ID)] == None :
#             endorsement[to_GTS_id][skill_ID] = [{'endorsement ID' : to_GTS_id + '_' + str(skill_ID) + '_' + from_GTS_id, 'endorser' : from_GTS_id}]
#             out_string = '%s has just endorsed %s in %s.' % (from_GTS_id, to_GTS_id, skill['Skill'][skill['Skill_ID'].index(skill_ID)])
#     elif from_GTS_id in [k['endorser'] for k in endorsement[to_GTS_id][str(skill_ID)]] :
#             out_string = '%s has already endorsed %s in %s.' % (from_GTS_id, to_GTS_id, skill['Skill'][skill['Skill_ID'].index(skill_ID)])
#     else :
#         endorsement[to_GTS_id][str(skill_ID)].append({'endorsement ID' : to_GTS_id + '_' + str(skill_ID) + '_' + from_GTS_id, 'endorser' : from_GTS_id})
#         out_string = '%s has just endorsed %s in %s.' % (from_GTS_id, to_GTS_id, skill['Skill'][skill['Skill_ID'].index(skill_ID)])
#     if 'has just endorsed' in out_string :
#         endorsement_ID = to_GTS_id + '_' + str(skill_ID) + '_' + from_GTS_id
#         endorsement_rating[endorsement_ID] = 1
#         
#         # print endorsement_refresh_table.keys()
#         p1 = endorsement_refresh_table[from_GTS_id][str(skill_ID)]['endorsed']
#         p2 = endorsement_refresh_table[to_GTS_id][str(skill_ID)]['endorsed']
#         # print p1
#         # print p2
#         
#         
#         endorsement_refresh_table[from_GTS_id][str(skill_ID)]['endorsed'] = p1 + 1.0
#         endorsement_refresh_table[to_GTS_id][str(skill_ID)]['received'] = p2 + 1.0
#         # print p1
#         # print p2
#         endorsement_refresh_table[from_GTS_id][str(skill_ID)]['skilled_connection'] = len([k for k in connection[from_GTS_id] if k in get_persons_with_skill(skill_ID)])
#         
#         n = endorsement_refresh_table[from_GTS_id][str(skill_ID)]['endorsed']
#         print(n)
#         c = endorsement_refresh_table[from_GTS_id][str(skill_ID)]['skilled_connection']
#         
#         p3 = skill_person[from_GTS_id]['rate'][skill_person[from_GTS_id]['skills'].index(skill_ID)]
#         skill_person[from_GTS_id]['rate'][skill_person[from_GTS_id]['skills'].index(skill_ID)] = p3 + 1.0/float(n)
#         endorsed = [k for k in connection[from_GTS_id] if k + '_' + str(skill_ID) + '_' + from_GTS_id in endorsement_rating.keys()]
#         IDs = [k + '_' + str(skill_ID) + '_' + from_GTS_id for k in endorsed]
#         for i in IDs :
#             endorsement_rating[i] = c / float(len(IDs))
#         edge_id = connection_graph.get_eid(from_GTS_id, to_GTS_id)
#         if connection_graph.es[edge_id]['weight'] == 0 :
#             connection_graph.es[edge_id]['weight'] = 0
#         else :
#             connection_graph.es[edge_id]['weight'] -= 1
#         profile[from_GTS_id]['Endorsements']['Endorsed'] += 1
#         profile[to_GTS_id]['Endorsements']['Received'] += 1
#     write_data()
#     return out_string

def get_aggregate_skill_rank(GTS_id, skill_ID) :
    read_data()
    # print endorsement_refresh_table[GTS_id][str(skill_ID)]
    n = endorsement_refresh_table[GTS_id][str(skill_ID)]['received']
    if n == 0 :
        r = skill_person[GTS_id]['rate'][skill_person[GTS_id]['skills'].index(skill_ID)]
    else :
        IDs = [GTS_id + '_' + str(skill_ID) + '_' + k
               for k in connection[GTS_id]
               if GTS_id + '_' + str(skill_ID) + '_' + k in 
               endorsement_rating.keys()]
        #IDs = [GTS_id + '_' + str(skill_ID) + '_' + k for k in endorser]
        r = 0.0
        for k in IDs :
            r += endorsement_rating[k]
        r += skill_person[GTS_id]['rate'][skill_person[GTS_id]['skills'].index(skill_ID)]
    return r

def get_connection_level(from_GTS_id, to_GTS_id) :
    read_data()
    path = connection_graph.get_shortest_paths(from_GTS_id, to_GTS_id)
    level = len(path[0]) - 1
    return level

def get_path(from_GTS_id, to_GTS_id) :
    read_data()
    all_path = connection_graph.get_all_shortest_paths(from_GTS_id, to_GTS_id)
    path = connection_graph.get_all_shortest_paths(from_GTS_id, to_GTS_id, weights='weight')
    named_all_path = [connection_graph.vs.select(k)['name'] for k in all_path]
    named_best_path = [connection_graph.vs.select(k)['name'] for k in path]
    named_all_without_source = []
    for k in named_all_path :
        a = k
        #a.remove(from_GTS_id)
        a.remove(to_GTS_id)
        named_all_without_source.append(a)
    named_best_without_source = []
    for k in named_best_path :
        a = k
        #a.remove(from_GTS_id)
        a.remove(to_GTS_id)
        named_best_without_source.append(a)
    output = {'all_short_paths' : named_all_without_source, 
             'best_path' : named_best_without_source}
    return output


read_data()
