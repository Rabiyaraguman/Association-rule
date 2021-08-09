import pandas as pd
import matplotlib.pyplot as plt
import streamlit as st
import pickle
import pickle
import plotly.express as px
import numpy as np
from sklearn.metrics import precision_score, recall_score
from wordcloud import WordCloud
from sklearn.preprocessing import LabelEncoder
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.naive_bayes import MultinomialNB
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import LinearSVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import cross_val_score
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import Normalizer
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics import confusion_matrix
from sklearn.metrics import plot_confusion_matrix, plot_roc_curve, plot_precision_recall_curve
from  sklearn.metrics import classification_report

st.set_option('deprecation.showPyplotGlobalUse', False)

model=pickle.load(open('model.pkl','rb'))
tfidf = pickle.load(open('clf.pkl', 'rb'))


def predict_forest(review):
    data = [review]
    vectorizer = tfidf.transform(data).toarray()
    prediction=model.predict(vectorizer)
    pred='{0:.{1}f}'.format(prediction[0], 2)
    return float(pred)

def main():
    st.write()
    st.title("Identifying side Effects and Evaluating the effectiveness of Drugs")
    st.sidebar.title("Drug Bar classifier")
    st.sidebar.subheader("Choose to get figures!")


    @st.cache(persist = True)
    def load_data():
        data = pd.read_csv('drugreviewprocessed.csv')
        tfidf = TfidfVectorizer(stop_words='english',ngram_range=(1,2))
        features = tfidf.fit_transform(data.cleanReview)
        labels   = data.vaderSentiment
        x_train,x_test,y_train,y_test = train_test_split(data['cleanReview'],data['ratingSentimentLabel'],random_state=0)
        return x_train,x_test,y_train,y_test, features, labels, data 


    def models_accuracy( x_train,x_test,y_train,y_test, features, labels ):
        models = [RandomForestClassifier(n_estimators=200,max_depth=3,random_state=0),LinearSVC(),MultinomialNB(),LogisticRegression(random_state=0,solver='lbfgs',max_iter=2000,multi_class='auto')]
        CV = 5
        cv_df = pd.DataFrame(index=range(CV * len(models)))
        entries = []
        for model in models:
            model_name = model.__class__.__name__
            accuracies = cross_val_score(model,features,labels,scoring='accuracy',cv=CV)
            for fold_idx,accuracy in enumerate(accuracies):
                entries.append((model_name,fold_idx,accuracy))
        cv_df = pd.DataFrame(entries,columns=['model_name','fold_idx','accuracy'])
        cv_df.groupby('model_name').accuracy.mean()
        return cv_df

    def postive_values(df):
        positive_vader_sentiments = df[df.ratingSentiment == 2]
        positive_string = []
        for s in positive_vader_sentiments.cleanReview:
            positive_string.append(s)
        positive_string = pd.Series(positive_string).str.cat(sep=' ')
        wordcloud = WordCloud(width=2000,height=1000,max_font_size=200).generate(positive_string)
        st.subheader("Positive Vader Sentiments")
        fig, ax = plt.subplots()
        im = ax.imshow(wordcloud,interpolation='bilinear')
        ax.axis('off')
        st.pyplot()
        

    def negative_values(df):
        negative_vader_sentiments = df[df.ratingSentiment == 1]
        negative_string = []
        for s in negative_vader_sentiments.cleanReview:
            negative_string.append(s)
        negative_string = pd.Series(negative_string).str.cat(sep=' ')
        wordcloud = WordCloud(width=2000,height=1000,max_font_size=200).generate(negative_string)
        st.subheader(" Negative Vader Sentiments")
        fig, ax = plt.subplots()
        ax.axis('off')
        im = ax.imshow(wordcloud,interpolation='bilinear')
        st.pyplot()

    def neutral_values(df):
      neutral_vader_sentiments = df[df.ratingSentiment == 0]
      neutral_string = []
      for s in neutral_vader_sentiments.cleanReview:
        neutral_string.append(s)
      neutral_string = pd.Series(neutral_string).str.cat(sep=' ')
      wordcloud = WordCloud(width=2000,height=1000,max_font_size=200).generate(neutral_string)
      st.subheader(" Neutral Vader Sentiments")
      fig, ax = plt.subplots()
      ax.axis('off')
      im = ax.imshow(wordcloud,interpolation='bilinear')
      st.pyplot()

    def svm_model(features, labels, df):
       class_names = ['postive', 'negative']
       model = LinearSVC('l2')
       x_train,x_test,y_train,y_test = train_test_split(features,labels,test_size=0.25,random_state=0)
       normalize = Normalizer()
       x_train = normalize.fit_transform(x_train)
       x_test = normalize.transform(x_test)
       model.fit(x_train,y_train)
       y_pred = model.predict(x_test)
       conf_mat = confusion_matrix(y_test,y_pred)
       st.subheader("Confusion Matrix")
       plot_confusion_matrix(model, x_test, y_test, display_labels = class_names)
       st.pyplot()
       accuracy = model.score(x_test, y_test)
       st.subheader("Accuracy")
       st.write("Accuracy: ", accuracy.round(2))
       st.subheader("Confusion Matrix")
       st.write(conf_mat)
       st.subheader("Classification Report")
       st.write(classification_report(y_test,y_pred,target_names= df['ratingSentimentLabel'].unique()))



    x_train,x_test,y_train,y_test, features, labels, data  = load_data()
    # svm_model(features, labels, data)
    # cv_df = models_accuracy(x_train,x_test,y_train,y_test, features, labels )
    # postive_values(data)
    # negative_values(data)
    def accuracy():
        cv_df = pd.read_csv("/home/talha/Downloads/Nasrin-streamlit/accuracy.csv")
        print(cv_df)
        da = cv_df.groupby('model_name').accuracy.mean()
        print(da)
        return da
    

    da_temp = """
    <div style="background-color:#025246 ;padding:10px">
    <h2 style="color:white;text-align:center;"> Accuracies of Different Model </h2>
    </div>
    """
   
    da_map = """
    <div style="background-color:#025246 ;padding:10px">
    <h2 style="color:white;text-align:center;"> Map of Accuracies </h2>
    </div>
    """
    

    html_temp = """
    <div style="background-color:#025246 ;padding:10px">
    <h2 style="color:white;text-align:center;"> Drugs Review ML </h2>
    </div>
    """
    st.markdown(html_temp, unsafe_allow_html=True)

    review = st.text_input("Review","Type Here")
    safe_html="""  
      <div style="background-color:#F4D03F;padding:10px >
       <h2 style="color:white;text-align:center;"> You have a postive review about this </h2>
       </div>
    """
    danger_html="""  
      <div style="background-color:#F08080;padding:10px >
       <h2 style="color:black ;text-align:center;"> You have a postive review about this</h2>
       </div>
    """

    if st.button("Predict"):
        output=predict_forest(review)
        if output == 2:
            st.success('Your reviw is postive ')
            st.markdown(safe_html,unsafe_allow_html=True)
        else:
            st.success('Your reviw is negative ')
            st.markdown(danger_html,unsafe_allow_html=True)

    classifier = st.sidebar.selectbox("Classifier", ("SVM Model Predict", "Postive Values", "Negative Values", "Model Accuracies"))
    # if classifier == "SVM Model Predict":
    #     svm_model(features, labels, data)


    # if classifier == "Postive Values":
    #     svm_model(features, labels, data)

    if st.sidebar.button("View Model"):
        svm_model(features, labels, data)

    if st.sidebar.button("Postive Values"):
         postive_values(data)

    if st.sidebar.button("Negative Values"):
      negative_values(data)

    if st.sidebar.button("Neutral Values"):
      neutral_values(data)

    if st.sidebar.button("Model Accuracies"):
      st.markdown(da_temp, unsafe_allow_html=True)
      sr = accuracy()
      x = list(sr.index)
      da = pd.DataFrame()
      da["Models Name"] = x
      da["Accuracies"] = sr.values
      print(da)
      st.dataframe(da.style.highlight_max(axis=0))
      print(da.columns)
      st.markdown(da_map, unsafe_allow_html=True)
      # st.map(da)
      # with st.echo(code_location='below'):
      fig = px.scatter(
              x=da["Models Name"],
              y=da["Accuracies"],
              )
      fig.update_layout(
                  xaxis_title="Models Name",
                  yaxis_title="Accuracies",
                  )
      st.write(fig)


if __name__=='__main__':
    main()