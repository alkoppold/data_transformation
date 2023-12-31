install.packages("preregr")
library(preregr)
# original: https://docs.google.com/document/d/1a2FJ_zRcvw-WHLB3E8jp_UDeWq2rWqy6hxBT2uP48M4/edit

title = "The landscape of Open Scholarship across disciplines - Phase 1 Literature Review",
authors = "Meng Liu, Alexander Hart, Aleksandra Lazić, Malgorzata Lagisz, Tamara Kalandadze, Bradley Baker, Mahmoud Elsherif, Flávio Azevedo",
target_discipline = "multi/interdisciplinary (i.e., no screening in terms of disciplines/research areas)",
tasks_and_roles = "The record of the tasks and roles of each individual is kept in this google sheet",

#Review methods
type_of_review = "bibliometric analysis (conceptual knowledge structure)",
review_stages = "Preparation, Search, Screening, Tagging, Synthesis
               Preparation: A group of analysts explored different literature databases and discussed the results to select the optimal strategy.   
               Search: Following the finalised search strategy, the lead analyst will perform the search and retrieve the data. 
               Screening: See details in this form.
               Tagging: See details in this form.
               Synthesis: See details in this form",
current_stage= "Preparation",
start_date = "2022-05-23",
end_date = "2022-12-01",
background =
  "Background of the Landscape project 
Researchers usually work in silos, without much awareness of or access to what's going on in other fields. The solitary nature of research often means that both within and across disciplines, the sharing of information or best practices relating to reproducible scientific methods is difficult. This can lead to a problem of reinventing the wheel whilst also resulting in knowledge that is biased or misleading (e.g., believing some finding is unique to a certain field when it is not). Collating an interdisciplinary database of Open Scholarship (OS) literature would help address this issue because it can remove the information barriers and expedite the accumulation of evidence. It is particularly necessary to build one for OS as it is still an emerging topic in many disciplines. Efforts are often isolated and scattered in fields where OS is still a lesser known topic. The lack of coordinated efforts across and within disciplines can give rise to many issues that hinder the progress of science. One commonly acknowledged issue is the jingle-jangle fallacy where the same terms are used to convey different meanings and the same meaning to different terms. For instance, preregistration can be defined as an uneditable and timestamped version of a research protocol, while in healthcare fields preregistration is a fast-track course that qualifies students into a medical profession. The Glossary project at FORRT is specifically targeted to address this issue by building one central hub with OS terms (Parsons et al., 2022). This project shares the same vision but works towards addressing the issue concerning the literature base. Through mapping the topics covered in academic OS literature, an up-to-date picture can be revealed, facilitating a transparent and timely understanding of current developments of OS in different fields. Therefore, it is important to make efforts to integrate diverse literatures, search for commonalities and uniqueness, identify gaps and areas of consensus. This initiative helps us consolidate what is being published about OS and sets us towards cumulative OS. Once a first version of this database has been established, it can be dynamically updated whenever new entries are necessary, possibly using more automated methods.

Phase 1 of the Landscape project
This preregistration form is for Phase 1 of the project described above. In Phase 1, we will build the base literature using Web of Science (WoS) and perform bibliometric analysis to synthesise the literature. This will be our initial step towards building an interdisciplinary database of OS literature and mapping the landscape of the OS literature. Depending on the results of the bibliometric analysis, we could follow up with in-depth narrative reviews and other complementary analyses.

Our workflow for Phase 1 is as follows:
Preregistration
Search strategy exploration (completed) 
Preregister on OSF
Data collection 
Piloting
Screening piloting 
Tagging piloting
Update preregistration based on piloting
Screening 
Tagging
Synthesis 
Independent analysis 
Final analysis
Writing up",
primary_research_question = "
What is the conceptual knowledge structure (e.g., themes and trends) of the OS literature?",
secondary_research_question = "What are the descriptive characteristics (e.g., number of publications, growth trajectory, top keywords) of the OS literature in different research fields?",
expectations_hypotheses = "N/A",
dvs_outcomes_main_vars = "N/A",
ivs_intervention_treatment = "N/A",
additional_variables = "N/A",
software = "R Studio, R version will be shown in the open code file",
funding = "None",
cois = "None",
overlapping_authorships = "The list of overlapping authorships will be updated after data collection.",


#Search strategy
databases = "Web of Science",
interfaces = "Web of Science",
grey_literature = "Not included",
inclusions_exclusion_criteria =
  "1. has open scholarship/research/science as the core topic (e.g., not mentioned in passing, or merely practising open science principles such as sharing data or following preregistration)
2. the keywords are provided in English (other languages could be added later on according to the coders’ language skills)",
query_strings =
  "TI = (“open science” OR “open research” OR “open scholarship”) OR AK = (“open science” OR “open research” OR “open scholarship”) OR KP = (“open science” OR “open research” OR “open scholarship”)",
search_validation_procedure =
  "the search should retrieve all of the target papers on the list:
Allen C., & Mehler D.M.A. (2019). Open science challenges, benefits and tips in early career and beyond. PLoS Biology 17(5), e3000246. https://doi.org/10.1371/journal.pbio.3000246

Evans, T. R., Pownall, M., Collins, E., Henderson, E. L., Pickering, J. S., O’Mahony, A., Zaneva, M., Jaquiery, M., & Dumbalska, T. (2022). A network of change: United action on research integrity. BMC Research Notes, 15(1). Scopus. https://doi.org/10.1186/s13104-022-06026-y

Kalandadze, T., & Hart, S. A. (2022). Open developmental science: An overview and annotated reading list. Infant and Child Development, e2334. https://doi.org/10.1002/icd.2334

Kowalczyk, O. S., Lautarescu, A., Blok, E., Dall’Aglio, L., & Westwood, S. J. (2022). What senior academics can do to support reproducible and open research: A short, three-step guide. BMC Research Notes, 15(1), 116. https://doi.org/10.1186/s13104-022-05999-0

Morey, R. D., Chambers, C. D., Etchells, P. J., Harris, C. R., Hoekstra, R., Lakens, D., ... & Zwaan, R. A. (2016). The Peer Reviewers' Openness Initiative: incentivizing open research practices through peer review. Royal Society Open Science, 3(1), 150547. https://doi.org/10.1098/rsos.150547

Nosek, B. A., Alter, G., Banks, G. C., Borsboom, D., Bowman, S. D., Breckler, S. J., Buck, S., Chambers, C. D., Chin, G., Christensen, G., Contestabile, M., Dafoe, A., Eich, E., Freese, J., Glennerster, R., Goroff, D., Green, D. P., Hesse, B., Humphreys, M., & Ishiyama, J.,... & Contestabile, M. (2015). Promoting an open research culture. Science, 348(6242), 1422–1425. https://doi.org/10.1126/science.aab2374

Parsons, S., Azevedo, F., Elsherif, M. M., Guay, S., Shahim, O. N., Govaart, G. H., ... & Aczel, B. (2022). A community-sourced glossary of open scholarship terms. Nature Human Behaviour, 6(3), 312-318. https://doi.org/10.1038/s41562-021-01269-4

Persson S., & Pownall M. (2021). Can open science be a tool to dismantle claims of hardwired brain sex differences? Opportunities and challenges for feminist researchers. Psychology of Women Quarterly, 45. 493-504. https://doi.org/10.1177/03616843211037613

Wagge, J. R., Brandt, M. J., Lazarevic, L. B., Legate, N., Christopherson, C., Wiggins, B., & Grahe, J. E. (2019). Publishing research with undergraduate students via replication work: The Collaborative Replications and Education Project. Frontiers in Psychology, 10, 247. https://doi.org/10.3389/fpsyg.2019.00247"

(contributors: Malgorzata Lagisz, Sandra Grinschgl, Bradley Baker, Aleksandra Lazić, Mahmoud Elsherif),
other_search_strategies = "None",
procedure_for_contacting_authors =
  "we will reach out to the corresponding authors by email in the case of missing abstract AND failure to retrieve full text on our end.",
results_of_contacting_authors = "We will update the results of contacting authors after the screening stage. If authors provide missing abstracts / full texts, these will be incorporated in the database.",
search_expiration_and_repetition = "We’ll conduct the search after the preregistration plan is uploaded to OSF and no repetition will be performed unless over a year has passed when we submit the manuscript.",
search_strategy_justification =
  "1. We choose WoS for its high-quality metadata (e.g.,research areas, journal indexing) as our initial database. Grey literature (e.g., OSF preprints, research in languages other than English) was considered during our exploration phase but we decided against it because the data structure is very different from WoS, most requiring manual downloading, which makes merging difficult and workload unmanageable. Our plan is to incrementally incorporate grey literature in later stages (e.g., as separate/complementary analysis).
2. We only use the three most general terms regarding open scholarship to keep our search results general, which allows for a more meaningful keyword co-occurrence analysis.
3. We only use title, author keyword and keywords plus as the search fields, to narrow down our search results. During the exploration stage, we tried including abstract as the search fields and the quality of the results were staggering, with some only mentioned OS in passing, which did not fit our inclusion criteria. Considering the volume of results was still large (> 7,000) after we excluded abstract from the search field, we believe this is a reasonable strategy.",
misc_search_strategy_details = "None",

#Screening
screening_stages = " 
Round 1: double coding/screening
In this round each article will be screened by two screeners. The screening will be based on title, author keywords, keywords plus, abstract and full text (if the previous fields are insufficient). Screeners will have three options: Include, Exclude, Uncertain 
Round 2: screening reconciliation
In this round only articles with inconsistent codes or at least one “Uncertain” code will be screened. A third coder will code the case and the majority vote will win. If the majority vote is still “Uncertain”, the lead analyst will make the final decision.",
screened_fields_masking = "Journal, author and publication years will be masked to minimise bias during the initial screening. If full text is required, the screeners will retrieve the full text which unblinds the masking. We will keep a record of which article required full text for screening.",
used_exclusion_criteria =
  "We will exclude articles that only mention  open science/research/scholarship in passing as well as those that only practise OS (e.g., making data open, following preregistration) or that use the terms as part of a phrase unrelated to OS (e.g., “open research issues”)",
screener_instructions = "Screeners will be instructed to review the title, abstract, and keywords of the articles yielded from the search. If they are unsure, they will be instructed to download the article and review it in full. They will be instructed to exclude every article that does not deal with the topics of open science/research/scholarship. We will provide definitions of these terms to the screeners.",
screening_reliability = "We will perform full double coding for the dataset: Each article will be screened by two coders.",
screening_reconciliation_procedure = "if two coders disagree, a third coder/arbitrator will code and the majority vote wins",
sampling_and_sample_size = "The final analysis will be conducted on the screened dataset.",
screening_procedure_justification = "We will pilot the screening process on a random sample (n=100) and refine the process accordingly. ",
misc_screening_details = "None",
screening_data_management_and_sharing = "the search results will be stored in bibTex format, which will then be processed in R and output as .csv file (e.g., assigning ID and extracting columns for screening). The screening and tagging will take place in shared google sheets.",
entities_to_extract = "In our case, the extraction stage is the tagging stage. We will extract or transform keywords in the following situations:  
For articles with missing author keywords: 
If the number of articles that require keywords tagging is beyond our capacity (specific number of coders to be updated later), we will use keywords plus (the automatically generated keywords by WoS) to replace the missing values.
If the number of articles that require keywords is within our capacity, we will manually extract keywords from title, abstract and keywords plus. 
Optional: We may also transform author keywords if in our keyword analysis we find the quality of author keywords in the dataset inadequate for identifying meaningful themes. To identify meaningful themes, we need a balance between common and unique keywords. If the keywords are largely the same in all the documents (i.e., one theme for all) or the keywords do not overlap between the documents (i.e., one theme for each article), we will need to transform the keywords to improve the quality. Specifically, we will manually refine author keywords based on a customised dictionary (e.g., merging unique keywords or refining common keywords). We will update the preregistration in later stages if we decide to take this option.",
extraction_stages = "
Round 1: double coding
In this round each article will be tagged by two coders. The extraction of keywords will be based on title, author keywords, keywords plus, and abstract. Coders will extract keywords, as opposed to generating/creating summative/interpretive keywords. If they find the available keywords in the text insufficient/not meaningful, they can flag the article and additional coders (including the lead analyst) will be involved to resolve the issue collectively.
Round 2: tagging reconciliation
In this round only articles with inconsistent codes will be tagged by a third coder. Codes extracted by two coders will be retained. Codes extracted by one coder will be removed.  
Round 3: transformation (optional)
Please refer to the above item for details of this option.",
extractor_instructions = "In the case of manually extracting/tagging author keywords, the coders will be instructed to extract keywords from the title, abstract, and keywords plus. They should extract as many keywords as there are in the specified fields. Coders will extract keywords, as opposed to generating/creating summative/interpretive keywords. If they find the available keywords in the text insufficient/not meaningful, they can flag the article and additional coders (including the lead analyst) will be involved to resolve the issue collectively.",
extractor_blinding = "None",
extraction_reliability = "In the case of manually extracting/tagging author keywords, two coders will be used and inconsistencies will be resolved by a third coder (codes with majority vote will be retained). ",
extraction_reconciliation_procedure = "if two coders disagree, a third coder/arbitrator will code and resolve the inconsistencies (codes with majority vote will be retained).",
extraction_procedure_justification = "If the total number of articles that require manual extraction is greater than 10, we will pilot the extraction process on a random sample and refine the process accordingly. The sample size will be 10% of the total number or 5, whichever is larger; if the total number of articles that requires manual extraction is less than or equal to 10, we will skip the pilot and employ the process on all articles that require manual extraction.",
extraction_data_management_and_sharing = "The google sheets used during the process will be shared openly.",
misc_extraction_details = "None"


#Synthesis and Quality Assessment
planned_data_transformations = “We will first run the analysis without transforming keywords and then evaluate the results. If the themes identified are not meaningful (due to too many/few common keywords), we will then create and use a customised dictionary of keywords (e.g., merging or further differentiating keywords via manual tagging) to improve the quality. Note that this will be a qualitative and exploratory process but we will record the decision-making process and provide justifications. ”          
missing_data = “Articles that cannot be manually tagged with keywords (e.g., missing author keywords, keywords plus, abstract AND full text) will be excluded from the analysis.”,
data_validation = “In our case, the quality of data mainly concerns the quality of the keywords, which can only be evaluated qualitatively (i.e., without quantitative standards of quality). Our manually tagged or transformed keywords will be shared openly, and we leave it for the reader to evaluate the quality of the keywords.”,
quality_assessment = “Considering this is a bibliometric analysis with exploratory aims, there are no quantifiable criteria for conclusions. We will follow our pre-registration and provide open data and materials to allow for the readers to evaluate the validity of our conclusions.”,
synthesis_plan = “We will use the Biblometrix package in R to perform the keyword analysis. The rest of the analyses will be performed in R as well, the specific packages will be updated later in the code files.”,
criteria_for_conclusions = “Considering this is a bibliometric analysis with exploratory aims, there are no quantifiable criteria for conclusions. We will follow our pre-registration and provide open data and materials to allow for the readers to evaluate the validity of our conclusions.”,
synthesis_masking = “The synthesis (i.e., keyword analysis in our case) will be performed by a small group of analysts with relevant knowledge/experience. The screening and keyword tagging phase will involve a larger number of analysts with no specific requirement on prior knowledge/experience (explicit instructions will be provided to all screeners/coders).”,
synthesis_reliability = “The analysts involved in the synthesis stage will first run the analysis independently and then discuss to decide the optimal and final solution.”,
synthesis_reconciliation_procedure = “Differences in synthesis results will be discussed and the solution with the majority vote will be the final solution.”,
sensitivity_analysis = “The separate/independent analyses conducted during the synthesis stage also serve the purpose of sensitivity analysis. The discrepancies in synthesis results will be taken into account in our interpretation and discussion of the results.”,
synthesis_procedure_justification = “Justifications will be updated based on the discussions among the analysts.”,
synthesis_data_management_and_sharing = “Our dataset and code files will be shared on OSF (link here).”,
misc_synthesis_details = “None.” 
