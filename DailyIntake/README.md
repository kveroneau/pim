## Daily Food Intake Program

A cross-platform desktop application to keep track of one's daily intake of
various needed nutrients.  Currently it is not fully operational, nor ready
for use.  It currently has the basic binary file format ready and various
forms to test out those components to ensure food and meals can be safely
stored and loaded back from your disk.

### This program will once completed provide the following details:

 * Ability to enter in individual food items with their nutrient values.
 * Ability to form individual meals using the added food items.
   - An example might be a fully made sandwich.
   - Another example might be a fully prepared cup of morning coffee.
   - It will be possible to see each meals full nutrient values based on the food provided.
 * Ability to track meals through multiple days.
   - The user will be-able to start the program on a given day and add meals to
     the current day with ease.
   - Each meal added to the current day will automatically begin to tally up
     each nutrient component from each of the food items in that given meal.
 * Additional features such as limit capping specific nutrients may be
   possible in a future version, where when the user adds a meal with say too
   much sodium over their daily cap, the application can easily present the
   user with a simple warning to let them know.
 * The ability to calculate an appropriate amount of sodium intake given an
   amount of potassium to counteract the sodium based on the 1:3 ratio.
 * Other future calculations may be added which are deemed useful.

### Future ideas:

 * Have a web-based UI for easily adding meals and viewing stats.
   - Will be written as a stand-alone HTTP Server application which can then
     be deployed to a compatible server alongside a compatible database file.
   - This will also be provided as a Docker image to be easily deployed.
 * A potential ncurses interface for running without a GUI from say an SSH
   session to also add meals and view stats.

