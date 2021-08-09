import pandas as pd
import matplotlib.pyplot as plt
import streamlit as st
import pickle
import pickle
from nltk.corpus import stopwords
import plotly.express as px
import numpy as np
import seaborn as sns
from sklearn.metrics import precision_score, recall_score
from wordcloud import WordCloud
from nltk.tokenize import word_tokenize
from nltk import ngrams
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
from collections import Counter

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
       a = classification_report(y_test,y_pred,target_names= df['ratingSentimentLabel'].unique())
       st.table(a)



    x_train,x_test,y_train,y_test, features, labels, data  = load_data()
    # svm_model(features, labels, data)
    # cv_df = models_accuracy(x_train,x_test,y_train,y_test, features, labels )
    def accuracy():
        cv_df = pd.read_csv("accuracy.csv")
        print(cv_df)
        da = cv_df.groupby('model_name').accuracy.mean()
        print(da)
        return da

    def data_visuliztion():
      df_train = pd.read_table("drugsComTrain_raw.tsv")
      df_test = pd.read_table("drugsComTest_raw.tsv",)
      merge = [df_train, df_test]
      df_data = pd.concat(merge)
      df_data.columns = ['Id','drugName','condition','review','rating','date','usefulCount']    #rename columns
      df_data = df_data.dropna(how = 'any', axis = 0)
      df_data.columns = df_data.columns.str.lower()
      # Sorting the dataframe
      df_data.sort_values(['id'], ascending = True, inplace = True)
      df_data.reset_index(drop = True, inplace = True)
      df_data.head(10)
      top_20_drugs_1 = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;"> Top 20 Drugs with 10/10 Rating </h2>
        </div>
        """
      st.markdown(top_20_drugs_1, unsafe_allow_html=True)
      # if vsdrop == "Top 20 Drugs with 10/10 Rating":
      sns.set(font_scale = 1.2, style = 'darkgrid')
      plt.rcParams['figure.figsize'] = [15, 8]
      rating = dict(df_data.loc[df_data.rating == 10, "drugname"].value_counts())
      drugname = list(rating.keys())
      drug_rating = list(rating.values())
      sns_rating = sns.barplot(x = drugname[0:20], y = drug_rating[0:20])
      sns_rating.set(title = 'Top 20 drugs with 10/10 rating', ylabel = 'Number of Ratings', xlabel = "Drug Names")
      plt.setp(sns_rating.get_xticklabels(), rotation=90);
      st.pyplot()
      top_20_drugs_2 = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;"> Top 20 Drugs with 1/10 Rating </h2>
        </div>
        """
      st.markdown(top_20_drugs_2, unsafe_allow_html=True)
      # if vsdrop == "Top 20 Drugs with 1/10 Rating":
      sns.set(font_scale = 1.2, style = 'whitegrid')
      plt.rcParams['figure.figsize'] = [15, 8]
      rating = dict(df_data.loc[df_data.rating == 1, "drugname"].value_counts())
      drugname = list(rating.keys())
      drug_rating = list(rating.values())
      sns_rating = sns.barplot(x = drugname[0:20], y = drug_rating[0:20], palette = 'winter')
      sns_rating.set(title = 'Top 20 drugs with 1/10 rating', ylabel = 'Number of Ratings', xlabel = "Drug Names")
      plt.setp(sns_rating.get_xticklabels(), rotation=90);
      st.pyplot()

      # A countplot of the ratings so we can see the distribution of the ratings
      dstr = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;"> Distribution of Ratings </h2>
        </div>
        """
      st.markdown(dstr, unsafe_allow_html=True)
      # if vsdrop == 'Distribution of Ratings':
      plt.rcParams['figure.figsize'] = [20,8]
      sns.set(font_scale = 1.4, style = 'whitegrid')
      fig, ax = plt.subplots(1, 2)
      sns_1 = sns.countplot(df_data['rating'], palette = 'spring', order = list(range(10, 0, -1)), ax = ax[0])
      sns_2 = sns.distplot(df_data['rating'], ax = ax[1])
      sns_1.set_title('Count of Ratings')
      sns_1.set_xlabel("Rating")
      sns_2.set_title('Distribution of Ratings')
      sns_2.set_xlabel("Rating");
      st.pyplot()


      # Word cloud of the reviews with rating equal to 10
      df_rate_ten = df_data.loc[df_data.rating == 10, 'review']
      k = (' '.join(df_rate_ten))
      wordcud12 = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;"> Word Cloud Ratings of Word with 10 </h2>
        </div>
        """
      st.markdown(wordcud12, unsafe_allow_html=True)
      # if vsdrop == 'Word Cloud Ratings of Word with 10':
      wordcloud = WordCloud(width = 1000, height = 500, background_color = 'white').generate(k)
      plt.figure(figsize=(15, 10))
      plt.imshow(wordcloud, interpolation="bilinear")
      plt.axis('off');
      st.pyplot()


      # Word cloud of the reviews with rating equal to 1
      wordcud22 = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;"> Word Cloud Ratings of Word  with 1 </h2>
        </div>
        """
      st.markdown(wordcud22, unsafe_allow_html=True)
      df_rate_one = df_data.loc[df_data.rating == 1, 'review']
      k1 = (' '.join(df_rate_one))
      # if vsdrop == 'Word Cloud Ratings of Word  with 1':
      wordcloud = WordCloud(width = 1000, height = 500).generate(k1)
      plt.figure(figsize=(15, 10))
      plt.imshow(wordcloud, interpolation="bilinear")
      plt.axis('off');
      st.pyplot()

      # converting the date into datetime format
      df_data['date'] = pd.to_datetime(df_data['date'], errors = 'coerce')
      # now extracting year from date
      df_data['Year'] = df_data['date'].dt.year
      # extracting the month from the date
      df_data['month'] = df_data['date'].dt.month
      # extracting the days from the date
      df_data['day'] = df_data['date'].dt.day
      # This barplot shows the mean rating of the drugs per year
      mean_rating = dict(df_data.groupby(df_data['date'].dt.year)['rating'].mean())
      # plt.rcParams['figure.figsize'] = [12, 7]
      # sns.set(font_scale = 1.2, style = 'whitegrid')
      yrely_ra = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;"> Yearly Ratings </h2>
        </div>
        """
      st.markdown(yrely_ra, unsafe_allow_html=True)
      # if vsdrop ==  'Yearly Ratings':
      sns_ = sns.barplot(x = list(mean_rating.keys()), y = list(mean_rating.values()), color = 'orange');
      sns_.set_xlabel("Year")
      sns_.set_ylabel("Rating");
      st.pyplot()
      # This barplot show the top 10 conditions the people are suffering.

      top_10_conditions = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;"> Top 10 conditions </h2>
        </div>
        """
      st.markdown(top_10_conditions, unsafe_allow_html=True)
      # if vsdrop == 'Top 10 conditions':
      cond = dict(df_data['condition'].value_counts())
      top_condition = list(cond.keys())[0:10]
      values = list(cond.values())[0:10]
      sns.set(style = 'darkgrid', font_scale = 1.3)
      plt.rcParams['figure.figsize'] = [18, 7]
      sns_ = sns.barplot(x = top_condition, y = values, palette = 'winter')
      sns_.set_title("Top 10 conditions")
      sns_.set_xlabel("Conditions")
      sns_.set_ylabel("Count");
      st.pyplot()
      # Top 10 drugs which are used for the top condition, that is Birth Control

      df = df_data[df_data['condition'] == 'Birth Control']['drugname'].value_counts()[0: 10]
      top_10_conditions_birth = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;"> Top 10 Drugs used for Birth Control </h2>
        </div>
        """
      st.markdown(top_10_conditions_birth, unsafe_allow_html=True)
      # if vsdrop == "Top 10 Drugs used for Birth Control":
      sns.set(font_scale = 1.2, style = 'darkgrid')
      sns_ = sns.barplot(x = df.index, y = df.values, palette = 'summer')
      sns_.set_xlabel('Drug Names')
      sns_.set_title("Top 10 Drugs used for Birth Control")
      plt.setp(sns_.get_xticklabels(), rotation = 90);
      st.pyplot()

      # Distribution of the useful count
      disturb_ofcon = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;"> Distribution of the useful count </h2>
        </div>
        """
      st.markdown(disturb_ofcon, unsafe_allow_html=True)
      # if vsdrop == "Distribution of the useful count":
      sns.set(style = 'whitegrid', font_scale = 1.3)
      plt.rcParams['figure.figsize'] = [12, 7]
      sns.distplot(df_data['usefulcount'].dropna())
      st.pyplot()

      df = df_data['date'].dt.year.value_counts()
      df = df.sort_index()

      per_year = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;"> Number of reviews per year </h2>
        </div>
        """
      st.markdown(per_year, unsafe_allow_html=True)
      # if vsdrop == "Number of reviews per year":
      sns_ = sns.barplot(x = df.index, y = df.values, color = 'mediumaquamarine')
      sns_.set_title("Number of reviews per year")
      sns_.set_xlabel("Year");
      st.pyplot()

      # Heatmap of the correlation matrix
      cor_mat = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;">Map of the correlation matrix </h2>
        </div>
        """
      st.markdown(cor_mat, unsafe_allow_html=True)
      # if vsdrop == "Map of the correlation matrix":
      plt.rcParams['figure.figsize'] = [7,5]
      sns.set(font_scale = 1.2)
      corr = df_data.select_dtypes(include = 'int64').corr()
      sns_heat = sns.heatmap(corr, annot = True, vmin=-1, vmax=1, center=0,
                  cmap=sns.diverging_palette(20, 220, n=200), square=True);
      plt.setp(sns_heat.get_xticklabels(), rotation = 45);
      st.pyplot()

      # Top 20 unigrams according to the rating
      df_ = df_data[['rating', 'review']]
      df_['review'] = df_data['review'].str.replace("&#039;", "")
      df_['review'] = df_['review'].str.replace(r'[^\w\d\s]',' ')
      # df_review_5 = " ".join(df_.loc[df_.rating <= 5, 'review'])
      # df_review_10 = " ".join(df_.loc[df_.rating > 5, 'review'])
      # token_review_5 = word_tokenize(df_review_5)
      # token_review_10 = word_tokenize(df_review_10)
      # unigrams_5 = ngrams(token_review_5, 1)
      # unigrams_10 = ngrams(token_review_10, 1)

      # frequency_5 = Counter(unigrams_5)
      # frequency_10 = Counter(unigrams_10)

      # df_5 = pd.DataFrame(frequency_5.most_common(20))
      # df_10 = pd.DataFrame(frequency_10.most_common(20))

      # Barplot that shows the top 20 unigrams
      # unigrams_20 = """
      #   <div style="background-color:#025246 ;padding:10px">
      #   <h2 style="color:white;text-align:center;">Top 20 unigrams according for rating</h2>
      #   </div>
      #   """
      # st.markdown(unigrams_20, unsafe_allow_html=True)
      # # if vsdrop == "Top 20 unigrams according for rating":
      # plt.rcParams['figure.figsize'] = [20,11]
      # fig, ax = plt.subplots(1,2)
      # sns.set(font_scale = 1.5, style = 'whitegrid')

      # sns_5 = sns.barplot(x = df_5[1], y = df_5[0], color = 'lightsteelblue', ax = ax[0])
      # sns_10 = sns.barplot(x = df_10[1], y = df_10[0], color = 'lightsteelblue', ax = ax[1])

      # # Setting axes labels
      # sns_5.set_title("Top 20 unigrams according for rating <= 5")
      # sns_10.set_title("Top 20 unigrams according for rating > 5")
      # sns_5.set_ylabel("Unigrams");
      # st.pyplot()

      # Top 20 bigrams according to the rating
      # bigrams_5 = ngrams(token_review_5, 2)
      # bigrams_10 = ngrams(token_review_10, 2)

      # frequency_5 = Counter(bigrams_5)
      # frequency_10 = Counter(bigrams_10)

      # df_5 = pd.DataFrame(frequency_5.most_common(20))
      # df_10 = pd.DataFrame(frequency_10.most_common(20))

      # Barplot that shows the top 20 bigrams
      # bigrams_20 = """
      #   <div style="background-color:#025246 ;padding:10px">
      #   <h2 style="color:white;text-align:center;">Top 20 bigrams according for rating</h2>
      #   </div>
      #   """
      # st.markdown(bigrams_20, unsafe_allow_html=True)
      # # if vsdrop == "Top 20 bigrams according for rating":
      # plt.rcParams['figure.figsize'] = [22,11]
      # fig, ax = plt.subplots(1,2)
      # sns.set(font_scale = 1.3, style = 'whitegrid')

      # sns_5 = sns.barplot(x = df_5[1], y = df_5[0], color = 'red', ax = ax[0])
      # sns_10 = sns.barplot(x = df_10[1], y = df_10[0], color = 'red', ax = ax[1])

      # # Setting axes labels
      # sns_5.set_title("Top 20 bigrams according for rating <= 5")
      # sns_10.set_title("Top 20 bigrams according for rating > 5")
      # sns_5.set_ylabel("bigrams");
      # st.pyplot()


      # Top 20 trigrams according to the rating
      # trigrams_5 = ngrams(token_review_5, 3)
      # trigrams_10 = ngrams(token_review_10, 3)

      # frequency_5 = Counter(trigrams_5)
      # frequency_10 = Counter(trigrams_10)

      # df_5 = pd.DataFrame(frequency_5.most_common(20))
      # df_10 = pd.DataFrame(frequency_10.most_common(20))

      # Barplot that shows the top 20 trigrams
      # trigrams_20 = """
      #   <div style="background-color:#025246 ;padding:10px">
      #   <h2 style="color:white;text-align:center;">Top 20 Trigrams according for rating</h2>
      #   </div>
      #   """
      # st.markdown(trigrams_20, unsafe_allow_html=True)
      # # if vsdrop == "Top 20 Trigrams according for rating":
      # plt.rcParams['figure.figsize'] = [25,13]
      # fig, ax = plt.subplots(1,2)
      # sns.set(font_scale = 1.3, style = 'whitegrid')

      # sns_5 = sns.barplot(x = df_5[1], y = df_5[0], color = 'orange', ax = ax[0])
      # sns_10 = sns.barplot(x = df_10[1], y = df_10[0], color = 'orange', ax = ax[1])

      # # Setting axes labels
      # sns_5.set_title("Top 20 trigrams according for rating <= 5")
      # sns_10.set_title("Top 20 trigrams according for rating > 5")
      # sns_5.set_ylabel("trigrams");
      # st.pyplot()


      # Removing the stop words before plotting
      stop_words = set(stopwords.words('english'))
      df_['review'] = df_['review'].str.lower()
      df_['review_1'] = df_['review'].apply(lambda x: " ".join(word for word in x.split() if word not in stop_words))
      df_review = " ".join(df_['review_1'])
      tokenize = word_tokenize(df_review)
      frequency = Counter(tokenize)
      df = pd.DataFrame(frequency.most_common(30))
      wor_cn = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;">Word Count Plot</h2>
        </div>
        """
      st.markdown(wor_cn, unsafe_allow_html=True)
      # if vsdrop == "Word Count Plot":
      plt.rcParams['figure.figsize'] = [12, 15]
      sns.set(font_scale = 1.3, style = 'whitegrid')

      # plotting
      word_count = sns.barplot(x = df[1], y = df[0], color = 'darkcyan')
      word_count.set_title("Word Count Plot")
      word_count.set_ylabel("Words")
      word_count.set_xlabel("Count");
      st.pyplot()
      # Giving the Sentiment according to the ratings
      df_data['sentiment_rate'] = df_data['rating'].apply(lambda x: 1 if x > 5 else 0)

      post_values = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;">Postive Sentiment Analysis</h2>
        </div>
        """
      st.markdown(post_values, unsafe_allow_html=True)
      postive_values(data)

      negt_values = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;">Negative Sentiment Analysis</h2>
        </div>
        """
      st.markdown(negt_values, unsafe_allow_html=True)
      negative_values(data)

      neyt_values = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;">Neutral Sentiment Analysis</h2>
        </div>
        """
      st.markdown(neyt_values, unsafe_allow_html=True)
      neutral_values(data)


    def drugs_effectiveness():
      data=pd.read_csv('effectiveandsideeffexts.csv')
      add_selectbox = st.sidebar.selectbox(
        "Enter or select the Drugname",
        (data['drugname'])
        )
      for i in range(data.shape[0]):
        data.rating[i]=eval(data.rating[i])
        data.effective[i]=eval(data.effective[i])
        data.ineffective[i]=eval(data.ineffective[i])
        data.condition[i]=eval(data.condition[i])
        data.Top_side_effect[i]=eval(data.Top_side_effect[i])
        data['Side_effect'][i]=eval(data['Side_effect'][i])

      g=list(data.loc[data['drugname']==add_selectbox,'condition'])[0]
      h=list(data[data['drugname']==add_selectbox]['rating'])[0]
      for i in range(len(h)):
        h[i]=int(float(h[i]*10))
      g=pd.DataFrame(g,columns=['Condition'])
      h=pd.DataFrame(h,columns=['Effectiveness'])
      j=pd.concat([g,h],axis=1)
      Q=pd.DataFrame(columns=['Condition','Effectiveness'])
      effective_cond = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;"> EFFECTIVE CONDITION </h2>
        </div>
        """
      st.markdown(effective_cond, unsafe_allow_html=True)
      if data.loc[data['drugname']==add_selectbox,'effective'].all():
        # st.info("EFFECTIVE CONDITION")
        for k in range(j.shape[0]):
          if j['Effectiveness'][k]>50:
            Q=pd.concat([Q,pd.DataFrame({'Condition':[j['Condition'][k]],'Effectiveness':[j['Effectiveness'][k]]})])
        Q=Q.reset_index(drop=True)
        Q.index+=1
        st.table(Q)


        Q=pd.DataFrame(columns=['Condition','Effectiveness'])
        ineffective_cond = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;"> INEFFECTIVE CONDITION </h2>
        </div>
        """
        # st.markdown(ineffective_cond, unsafe_allow_html=True)
        if data.loc[data['drugname']==add_selectbox,'ineffective'].all():
          # st.info('INEFFECTIVE CONDITION')
          for k in range(j.shape[0]):
            print(k)
            if j['Effectiveness'][k]<=50:
              Q=pd.concat([Q,pd.DataFrame({'Condition':[j['Condition'][k]],'Effectiveness':[j['Effectiveness'][k]]})])
          Q=Q.reset_index(drop=True)
          print(Q)
          Q.index+=1
          st.table(Q)
        side_effects = """
        <div style="background-color:#025246 ;padding:10px">
        <h2 style="color:white;text-align:center;"> TOP SIDE EFFECTS </h2>
        </div>
        """
        st.markdown(side_effects, unsafe_allow_html=True)
        if data.loc[data['drugname']==add_selectbox,'Top_side_effect'].all():
          # st.info('TOP SIDE EFFECTS')
          R=pd.DataFrame(list(data.loc[data['drugname']==add_selectbox,'Top_side_effect'])[0],columns=['Top_side_effect'])
          R.index+=1
          st.table(R)

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

    classifier = st.sidebar.selectbox("Main Classes", ("Chose one of them","SVM Model Predict", "Data Visuallization", "Model Accuracies", "Effectiveness And Side Effects"))
    if classifier == "SVM Model Predict":
      svm_model(features, labels, data)

    if classifier == "Data Visuallization":
      data_visuliztion()

    if classifier == "Effectiveness And Side Effects":
      drugs_effectiveness() 


    if classifier == "Model Accuracies":
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

    if st.sidebar.checkbox("Show raw data", False):
      st.subheader("Drugs Data Set (Clean)")
      st.write(data.head(100))

      
      




if __name__=='__main__':
    main()