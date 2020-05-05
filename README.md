# Introduction 
**rappbot** is an R wrapper for the Appbot API

# Useful links
- [Appbot](https://appbot.co/)
- [Appbot API](https://app.appbot.co/api)

# Authentication

To authenticate, you need to create an API key in the console. Then you authenticate like this:

`appbot_auth(user,password)`

This will save the authentication credentials to your environment.

# Sources

Getting a dataframe of your available sources is easy.

`sources <- appbot_sources()`

This will give you the source ID you will need to be able to access other services.

# Ratings

To call the ratings, we have different methods for Android and iOS.

For Android, the following:

`android_reviews <- appbot_ratings_android(app_id, method, start = NULL, end = NULL)`

Here's an example:

```
start_date = '2020-03-26'
end_date = Sys.Date()-2

droid_ratings <- appbot_ratings_android(droid_id,'historical',start_date,end_date)
```

For iOS:

`ios_reviews <- appbot_ratings_ios(app_id, method, output, country, start = NULL, end = NULL)`

The reason for this is that there are differences in the way the data is structured between the two OS types.

# Reviews

`reviews <- appbot_reviews(app_id)`

There are many different options for query parameters to add to the reviews call, which is why they are passed in the ellipse. You need to refer to the documentation to see how the parameters are described, and then add them in the appropriate way, for instance if you wanted to get reviews from Germany, you would pass 'country=DE' as an argument.

Example:

```
s <- paste('start',start_date,sep='=') # returns "start=2020-03-26"
e <- paste('end',end_date,sep = '=')

droid_reviews <- appbot_reviews(droid_id,5,s,e)
```

An additional option would be to pivot the topics column (which is returned as a list column) out into individual columns each, like this:

`droid_reviews_pivoted <- appot_reviews(droid_id,pivoted=TRUE)`

# Other tools:

* `appbot_countries` - Get a dataframe of countries for which data is available
* `appbot_languages` - Get a dataframe of available languages for reviews
* `appbot_review_summary` - Get different types of review summaries
* `appbot_reviews_import` - Import reviews into a custom source
* `appbot_sentiment` - Get a dataframe of review sentiments - positive, negative, neutral and mixed
* `appbot_tags` - Get a dataframe of review counts using your custom tags
* `appbot_topics_reviews` - Get reviews by topic
* `appbot_topics` - Get a dataframe of review counts by topics (using either Appot's automatic topic classification, or your own custom topics)
* `appbot_review_summary` - Get different types of review summaries
* `appbot_versions` - Get a dataframe of review counts by app version (iOS only)
* `appbot_words` - Get a dataframe of the frequency of certain keywords in your reviews (multiple methods available)