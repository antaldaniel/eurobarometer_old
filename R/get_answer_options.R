#' Get answer options from trend questions.
#'
#' @param language Defaults to \code{"en"}. Limited ability to test
#' in \code{"hu"}.
#' @examples
#' get_answer_options()
#' @export

get_answer_options <- function ( language = "en") {

  if ( language == "en") {
    answer_options <- list (
      multiple_choice = c("Mentioned", "Not mentioned"),

      indicator = c("Not mentioned"),

      recale_acceptable_3 = c("Never acceptable", "Sometimes acceptable",
                              "Always acceptable"),
      rescale_agreement_4 = c("Totally agree", "Tend to agree",
                              "Tend to disagree", "Totally disagree"),
      rescale_alphanumeric = c("None", "One", "Two"),
      rescale_amount_3 = c("Too much", "About the right amount",
                           "Too little"),
      rescale_attachment_4 = c( "Very attached", "Fairly attached",
                                "Not very attached",
                                "Not at all attached"),
      rescale_benefit_2 = c("Would benefit",
                            "Would not benefit"),
      rescale_better_worse_3 = c("Better",
                                 "Same",
                                 "Worse"),
      rescale_description = c("Very well",
                              "Fairly well",
                              "Fairly badly",
                              "Very badly"),
      rescale_difficulty =  c("Most of the time",
                              "From time to time",
                              "Almost never/never"),
      rescale_direction = c("right direction",
                            "wrong direction",
                            "neither"),
      rescale_effective_4 = c( "Very effective", "Fairly effective",
                               "Not very effective", "Not at all effective"),
      rescale_for_against_2 = c("For", "Against"),

      rescale_for_against_4 = c("Strongly in favour", "Fairly in favour" ,
                                 "Strongly opposed", "Fairly opposed"),

      rescale_goal_3 = c("Too ambitious",
                         "About right",
                         "Too modest"),
      rescale_good_bad_3 = c("A good thing",
                             "Neither good nor bad",
                             "A bad thing"),
      rescale_image = c("Very positive",
                        "Fairly positive",
                        "Fairly negative",
                        "Very negative"),
      rescale_informed_4 = c( "Very well informed", "Fairly well informed",
                              "Not very informed", "Not at all informed"),
      rescale_likely_4 = c("Fairly likely", "Not very likely",
                            "Not at all likely", "Very likely"),
      rescale_low_strong_3 = c("Low", "Medium", "Strong"),

      rescale_low_strong_4 = c("Not at all", "Low", "Medium", "Strong"),

      rescale_optimism_4 = c( "Very optimistic", "Fairly optimistic",
                              "Fairly pessimistic", "Very pessimistic"),

      rescale_much_little_3 = c("Enough", "Too little", "Too much"),

      rescale_political_interest = c("Not at all interested in politics",
                                     "Slightly interested in politics",
                                     "Moderately interested in politics",
                                     "Strongly interested in politics"),

      rescale_poor_high_4 = c("Very high", "Poor", "High",
                               "Very poor / none"),
      rescale_situation = c("Very good",
                            "Rather good",
                            "Rather bad",
                            "Very bad"),
      rescale_satisfaction = c("Very satisfied",
                               "Fairly satisfied",
                               "Not very satisfied",
                               "Not at all satisfied"),
      rescale_social_class_en =  c("The working class of society",
                                   "The middle class of society",
                                   "The upper class of society"),
      rescale_subjective_urbanization = c("Rural area or village",
                                          "Small or medium-sized town",
                                          "Large town/city"),
      rescale_time_frequency_3 = c("frequently", "occasionally",
                                   "never"),
      rescale_time_frequency_use = c("Everyday/Almost everyday",
                                     "Two or three times a week",
                                     "About once a week",
                                     "Two or three times a month",
                                     "Less often", "Never"),
      rescale_trust = c("Tend to trust",
                        "Tend not to trust"),
      rescale_yes_no_2 = c("Yes", "No"),
      rescale_yes_no_3 = c("Yes, on several occasions",
                           "Yes, once or twice",
                           "No"),
      rescale_yes_no_4 = c("Yes, to some extent", "Yes, definitely",
                           "No, definitely not", "No, not really"),
      stringsAsFactors = FALSE
    )
  }
  if ( language == "hu") {
    answer_options <- list (
      rescale_better_worse_3 = c("Jobb lesz",
                                 "Ugyanolyan lesz",
                                 "Rosszabb lesz"),
      rescale_yes_no_2 = c("Igen", "Nem"),
      stringsAsFactors = FALSE
    )
  }
  return (answer_options)
}



