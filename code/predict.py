import pickle 

loaded_model = pickle.load(open('results/ad1.pkl', 'rb'))

def predict(features):
    return list(loaded_model.predict_proba(features)[:,1])