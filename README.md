# Using Every Door Direct Mail Web Push Surveys and Multi-level modelling with Post Stratification to estimate Perceptions of Police at Small Geographies

Team MCHawks (Giovanni Circo and Andrew Wheeler)

Code and resources for **[2023 NIJ competition](https://nij.ojp.gov/funding/innovations-measuring-community-perceptions-challenge)**

# Overview of Method

We suggest using [USPS Every Door Direct Mail (EDDM)](https://eddm.usps.com/eddm/select-routes.htm) to send post-card mailers to individuals on a postal route. These mailers will then have a QR code and a plain text url in which the recipient can take the survey. 

Then we suggest using multi-level regression and post-stratification to adjust for differential survey non-response, and generate spatial estimates of attitudes towards police.

See the MainNarrative.docx file (in the writeup folder) for a fuller description. But our final estimates are this approach will cost approximately $7.25 per completed survey, so a city can implement this approach with a budget of under $10,000 and likely have a reasonably large enough sample size to generate micro-spatial estimates.

# Example Demonstration Push Surveys

We have provided an example of using query strings to be able to push similar surveys, but have them save the endpoint so you can determine the specific postal route a survey was delivered to.

 - [Survey1](https://crimede-coder.com/graphs/survey?surv=se1)
 - [Survey2](https://crimede-coder.com/graphs/survey?surv=se2)

This links on the backend to a googlesheet to cache responses, so is effectively free to deploy on a website.

# Criteria for Judging Surveys in the NIJ Competition

 - Representative– entries accurately represent the characteristics of the community on the key dimensions of race, ethnicity, age, and gender.
 - Cost effective – so that they can be deployed frequently to better understand patterns in changes in community perceptions.
 - Accurate across microgeographies – produce accurate estimates across microgeographies (i.e., the smallest unit that does not reveal the identities of any individuals) to reveal differences in patterns of perceptions.
 - Frequent – capable of low-burden administration on a systematic basis.
 - Scalable – able to be successfully deployed in jurisdictions of varying sizes.

# Example Demo MRP with Raleigh data

We have provided a demonstration of using MRP to a set of survey data in [Raleigh](https://data-ral.opendata.arcgis.com/datasets/community-survey-fy18/explore), see the `mrp_example` folder.