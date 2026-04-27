module Samples (module Samples) where

import Data.Ollama.Chat
import DocumentTypes.CV

testContextCV :: ContextCV
testContextCV =
  ContextCV
    { ctxModelName = "DUMMY_MODEL",
      ctxCVLocation = mkInstr "This is a location" "keep it as is",
      ctxCVContext = mkInstr "This is a resume" "",
      ctxSummary =
        mkSection
          "Section Summary"
          (mkInstr "This is the summary" "rephrase the summary, use past tense"),
      ctxObjective =
        mkSection
          "Section Objective"
          (mkInstr "This is the objective" "rephrase the objective"),
      ctxProgrammingLanguage = mkSection "Section Programming Languages" (),
      ctxTechnologies = mkSection "Section Technologies" (),
      ctxStandards = mkSection "Section Standards" (),
      ctxPractices = mkSection "Section Practices" swePracticeContext,
      ctxSpokenLanguages =
        mkSection
          "Section Spoken Languages"
          (ContextSpokenLanguage $ mkInstr "This is a spoken language" "keep it as is"),
      ctxWorkExperience =
        mkSection
          "Section Work Experience"
          ( ContextWorkExperience
              (mkInstr "This is the location of an experience" "keep it as is")
              ( ContextExperienceType
                  { ctxEmployee =
                      ContextEmployee
                        { ctxeEmployerDescription =
                            mkInstr "This is an employer description" "rephrase it make it cooler",
                          ctxeResponsibilities =
                            ContextResponsibility
                              { ctxrespDescription =
                                  mkInstr
                                    "This is a description of a responsibility"
                                    "rephrase it make it nice",
                                ctxrespPractice = swePracticeContext
                              }
                        },
                    ctxSelfEmployed =
                      ContextSelfEmployed
                        { ctxseCustomer =
                            ContextCustomer
                              { ctxcuCustomerDescription =
                                  mkInstr
                                    "This is a customer description"
                                    "rephrase it make it sound groovy",
                                ctxcuResponsibility =
                                  ContextResponsibility
                                    { ctxrespDescription =
                                        mkInstr
                                          "This is a description of a responsibility"
                                          "rephrase it make it funky",
                                      ctxrespPractice = swePracticeContext
                                    }
                              }
                        }
                  }
              )
          ),
      ctxEducation =
        mkSection
          "Section Education"
          ( ContextEducation
              { ctxeduDegree = mkInstr "This is a degree" "keep it as is",
                ctxeduLocation = mkInstr "This is the location of the university" "keep it as is",
                ctxeduDescription =
                  ContextEducationDescription
                    { ctxeddDescription =
                        mkInstr
                          "This is an educational description. Behave as a student."
                          "keep it as is",
                      ctxeddSWEPractices = swePracticeContext
                    }
              }
          ),
      ctxAwards =
        mkSection
          "Section Awards"
          (ContextAward $ mkInstr "This is the description of an award" "rephrase it in past tense")
    }
  where
    mkSection section = ContextSection (mkInstr section "Rephrase the section")
    swePracticeContext =
      ContextSWEPractice $
        mkInstr "This is a software practice" "Rephrase the practice"

mkResult :: MD5Hash -> LLMPromptResultText
mkResult inputHashMD5 = LLMPromptResultText {..}
  where
    getPromptResultText = "TEST SUITE"

cacheC :: CV LLMPromptResultText
cacheC = CV {..}
  where
    currentLocation = mkResult "973f2ec338adf7ce8949fbdbe1316568"
    cvSummary =
      Section
        (mkResult "df5de9da403f24fbd516df1a956f1580")
        (mkResult "fc1d26ed738016cab31abcd999ef23c6")
    cvObjective =
      Section
        (mkResult "77781de7debf1bee3a0dbe5c0a0b4e86")
        (mkResult "2b88bc79f94645cc655d942587efcce9")
    cvProgrammingLanguages =
      Section
        (mkResult "385992a22192719f72b043796727eb0c")
        [ProgrammingLanguage Nothing "PL1"]
    cvTechnologies = Section (mkResult "ccf41796cf31cfcc45a3e8f803951272") []
    cvStandards = Section (mkResult "dee0a0e03b140fa5d47fdad24996220c") []
    cvPractices = Section (mkResult "6d2adeabf921183dffb545a4d97b5869") []
    cvSpokenLanguages =
      Section
        (mkResult "b50977e9d89967b41988b45c64595706")
        [SpokenLanguage (mkResult "6cb11e7c8ce3a2edf975aefdc4cedf3f")]
    cvWorkExperience =
      Section
        (mkResult "96e744de5465493e82221964babfac2a")
        [ WorkExperience
            { startTime = TimePoint 2022 03,
              endTime = TimePoint 2025 02,
              position = "POSITION",
              employer = "EMPLOYER",
              workLocation = mkResult "f5c5ee655762b87916abd1e4196d290c",
              job =
                EmployeeExperience $
                  Employee
                    { employerDescription = mkResult "c456055381abca892a44809e634fa01a",
                      employerResponsibilities =
                        [ Responsibility
                            { description = mkResult "62483a9be2bc3c6ef721e288e76cbf5e",
                              languages = [ProgrammingLanguage Nothing "PL1"],
                              technologies = [],
                              standards = [],
                              practices = []
                            }
                        ]
                    }
            },
          WorkExperience
            { startTime = TimePoint 2022 03,
              endTime = TimePoint 2025 02,
              position = "POSITION 2",
              employer = "EMPLOYER 2",
              workLocation = mkResult "d951472db18b5466448765fdbacaf1a4",
              job =
                SelfEmployedExperience
                  ( SelfEmployed
                      { customers =
                          [ Customer
                              { customerDescription = mkResult "fa6b149b9ad6da6f47c771f2c49cf683",
                                customerResponsibilities =
                                  [ Responsibility
                                      { description = mkResult "c2f6f2259c50bed3b40fffc9e690bfdc",
                                        languages = [ProgrammingLanguage Nothing "PHP"],
                                        technologies = [],
                                        standards = [],
                                        practices = []
                                      }
                                  ]
                              }
                          ]
                      }
                  )
            }
        ]
    cvEducation = Section (mkResult "7997288d862feb615e40b65cd889be8a") []
    cvAwards = Section (mkResult "24788a34f26fdcec84917c4b9d3da06e") []

documentT :: CV TextAtom
documentT = CV {..}
  where
    currentLocation = "Milano"
    cvSummary = Section "Summary" "SUMMARY BODY"
    cvObjective = Section "Objective" "OBJECTIVE BODY"
    cvProgrammingLanguages = Section "Programming languages" [ProgrammingLanguage Nothing "PL1"]
    cvTechnologies = Section "Technologies" []
    cvStandards = Section "Standards" []
    cvPractices = Section "Practices" []
    cvSpokenLanguages = Section "Spoken languages" ["SPOKEN LANGUAGE 1"]
    cvWorkExperience =
      Section
        "Work Experience"
        [ WorkExperience
            { startTime = TimePoint 2022 03,
              endTime = TimePoint 2025 02,
              position = "POSITION",
              employer = "EMPLOYER",
              workLocation = "LOCATION",
              job =
                EmployeeExperience $
                  Employee
                    { employerDescription = "EMPLOYER DESCRIPTION",
                      employerResponsibilities =
                        [ Responsibility
                            { description = "RESP1",
                              languages = [ProgrammingLanguage Nothing "PL1"],
                              technologies = [],
                              standards = [],
                              practices = []
                            }
                        ]
                    }
            },
          WorkExperience
            { startTime = TimePoint 2022 03,
              endTime = TimePoint 2025 02,
              position = "POSITION 2",
              employer = "EMPLOYER 2",
              workLocation = "LOCATION 2",
              job =
                SelfEmployedExperience
                  ( SelfEmployed
                      { customers =
                          [ Customer
                              { customerDescription = "CUSTOMER DESCRIPTION",
                                customerResponsibilities =
                                  [ Responsibility
                                      { description = "RESPONSIBILITY 2",
                                        languages = [ProgrammingLanguage Nothing "PHP"],
                                        technologies = [],
                                        standards = [],
                                        practices = []
                                      }
                                  ]
                              }
                          ]
                      }
                  )
            }
        ]
    cvEducation = Section "Education" []
    cvAwards = Section "Awards" []

-- same as documentT but with a changed section title. the summary body is identical.
documentT1 :: CV TextAtom
documentT1 = documentT {cvSummary = cvSummary}
  where
    cvSummary = Section "Summary Title 2" "SUMMARY BODY"

-- | Expected result of running documentT1 through preparePrompts using cacheC
resultR1 :: CV LLMPromptParameters
resultR1 =
  pureCacheResult
    { cvSummary =
        (cvSummary pureCacheResult)
          { -- defaultChatOps can be used because ShallowEq doesn't compare the ChatOps between
            -- two RunOllamaPrompt constructors.
            sectionHeading = RunOllamaPrompt defaultChatOps "ecdf17f53f2cdb80f4a3b906e3b1c18d"
          }
    }
  where
    pureCacheResult = fmap fromCache cacheC
    fromCache :: LLMPromptResultText -> LLMPromptParameters
    fromCache LLMPromptResultText {..} = KeepCachedResult getPromptResultText inputHashMD5

-- same as documentT but with an added list item (Practice)
documentT2 :: CV TextAtom
documentT2 = documentT {cvPractices = cvPractices}
  where
    cvPractices = Section "Practices" ["Practice"]

-- | Expected result of running documentT2 through preparePrompts using cacheC
resultR2 :: CV LLMPromptParameters
resultR2 =
  pureCacheResult
    { cvPractices =
        (cvPractices pureCacheResult)
          { sectionBody =
              [ SWEPractice $ RunOllamaPrompt defaultChatOps "a15b7f1ecf3172a626d87d9f20e961f7"
              ]
          }
    }
  where
    pureCacheResult = fmap fromCache cacheC
    fromCache :: LLMPromptResultText -> LLMPromptParameters
    fromCache LLMPromptResultText {..} = KeepCachedResult getPromptResultText inputHashMD5
