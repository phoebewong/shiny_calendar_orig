Introduction
=======================

This Shiny app aims to visualize your Microsoft Outlook Calendar Data. The sample data shown in the app is simulated data from Outlook Calendar. 
* If you wish to visualize the data of your Microsoft Outlook Calendar, you will need to import a csv file of your Calendar.


To download a csv file of your calendar history from Microsoft Outlook, follow [here](http://tinnes.co.uk/desktopcalendar/support/7/faq_importexport_exportoutlook2010.php) and sorry, the export functionality does **not seem to support Mac users** at the moment.
After downloading the csv file, you can upload it to the app to visualize the output.

---

## Summary
### Left Side Bar:
1. **CSV Upload**: you can upload your Outlook data here, by following the link above or [here](http://tinnes.co.uk/desktopcalendar/support/7/faq_importexport_exportoutlook2010.php) 
2. **My name**: the default name chosen comes from your data, if it is not your name, you can change it by selecting the right name on the dropdown menu. 
- It is important to have the right name for plots under the "People" tab
3. **Columns to show**: columns to show on the "Table" tab
4. **Date input**: pick the range of dates that you are interested in analzying
5. **Exclude All Day Events**: checked by defult, and it is helpful especially for those of you who also uses the calendar as a project management tool by creating "All Day Events"


### Tabs: 
1. **Table**: shows a table of your meetings, you can pick the columns on the left side bar. You can also search on the top right.
2. **Summary**: shows some basic summary statictics of your calendar data with plots on number of meetings and average duration of meetings per month.
3. **People**: shows plots related to people who sent you a meeting invite and who had meetings with you
4. **Time**: interactive heatmap of meeting frequency, with the most 
5. **Network**: two network graphs of those who had meetings with you. For the static network graph, the Size of the nodes represents number of meetings you had together, while the width of the linkages representes number of meetings the two nodes/people had together.

---

