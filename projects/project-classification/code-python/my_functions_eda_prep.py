# standard libraries
import numpy  as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

#----------------------------------------------------------------------------------------------------
# Explore Data Functions
#----------------------------------------------------------------------------------------------------

#------------------------------------------------------------
# cursory_exploration
#------------------------------------------------------------
def cursory_exploration(df, dtypes=False):
    """
    Print information from cursory exploration - shape, data types, missing values
    
    Parameters:
    df (DataFrame): to explore
    dtypes (bool): include output on data types
    
    Returns: None
    """
    #------------------------------------------------------------
    # cursory exploration
    #------------------------------------------------------------
    print('Shape: ', df.shape)

    # dtypes displays columns names, so I don't need to do it explicitly
    #print('\nColumn Names:')
    #print(df.columns)

    if (dtypes):
        print('\nData Types:')
        print(df.dtypes)

    #------------------------------------------------------------
    # check for missing values
    #------------------------------------------------------------
    print('\nMissing Values:')
    missing_s = df.isnull().sum()

    if ( missing_s.sum() == 0 ):
        print("No missing values detected")
    else:
        print(missing_s[missing_s>0])

        
#------------------------------------------------------------
# function ...
#------------------------------------------------------------
def countplot_by_X(df, column_name, order=None):
    
    g=sns.countplot(x=column_name, data=df, order=order)
    
    ax=g.axes
    for p in ax.patches:
        ax.annotate(f"{p.get_height() * 100 / df.shape[0]:.1f}%",

                    (p.get_x() + p.get_width() / 2.0, p.get_height()),

                    ha='center', va='center', fontsize=10, color='gray',
                    rotation=0, xytext=(0,6), textcoords='offset points'
                   )
        
def explore_target_variable(df, target_col_name, order=None):
    """
    Explore target variable by printing count & proportion of the values.  Also plot a bar chart.
    
    Parameters:
    df (DataFrame): containing target variable
    target_col_name (str): name of target variable column
    
    Returns: None
    """

    targetEDA = df.groupby(target_col_name).size()
    print(pd.DataFrame({'Count': targetEDA, 'Proportion': targetEDA / df.shape[0]}))
    
    countplot_by_X(df, target_col_name, order)

#------------------------------------------------------------
# function to generate crosstab based on proportion rather than frequency
#------------------------------------------------------------
def crosstab_prop(df, cat_feature_name, target_name):
    """
    Create crosstab with proportion of row vs. frequency.
    Outputs textual crosstab and stacked bar plot of proportion.
    
    Parameters:
    df (DataFrame): containing target variable
    cat_feature_name (str): name desired feature
    
    Returns: None
    """
    xtab_data = pd.crosstab(df[cat_feature_name], df[target_name])
    xtab_data_prop = xtab_data.divide(xtab_data.sum(axis=1), axis=0)
    print(xtab_data_prop)
    xtab_data_prop.plot(kind='bar', stacked=True, grid=False)


    
#----------------------------------------------------------------------------------------------------
# Data Preprocessing Functions
#----------------------------------------------------------------------------------------------------

#------------------------------------------------------------
# move_target_variable
#------------------------------------------------------------
def move_target_variable(df, target_col_name):
    """
    Moves target variable named (target_col_name) to first position in given DataFrame (df)
    
    Parameters:
    df (DataFrame): containing target variable
    target_col_name (str): name of target variable column
    
    Returns: None
    """
    targetSeries = df[target_col_name]
    # remove target from current location and insert in column 0
    del df[target_col_name]
    df.insert(0, target_col_name, targetSeries)
    

    
    
    
def convert_to_dummies(df, start_idx=1):
    # perform data transformation. Creates dummies of any categorical feature
    for col in df.columns[start_idx:]:
        attName = col
        dType = df[col].dtype
        # create dummies)
        if dType == object:
            df = pd.concat([df, pd.get_dummies(df[col], prefix=col)], axis=1)
            del df[attName]
    return df
