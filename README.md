README
================
Zack Vaskalis
6/12/2020

  - [Reading and Summarizing JSON
    Datasets](#reading-and-summarizing-json-datasets)
      - [The power of JSON](#the-power-of-json)
          - [What is JSON?](#what-is-json)
          - [JSON Data Types](#json-data-types)
          - [So where is JSON used?](#so-where-is-json-used)
          - [A good way to store data?](#a-good-way-to-store-data)
      - [Possible R Packages for JSON](#possible-r-packages-for-json)

# Reading and Summarizing JSON Datasets

A Vignette using National Hockey League (NHL) Application Programming
Interface (API) - No “Hat Tricks” here\!

<img src="https://upload.wikimedia.org/wikipedia/commons/c/c9/JSON_vector_logo.svg" width="200px" />

## The power of JSON

### What is JSON?

Before we can begin investigating the datasets provided by the [NHL
Records ‘Franchise’
API](https://gitlab.com/dword4/nhlapi/-/blob/master/records-api.md), we
must first discuss what JSON is and what tools do we have at our
disposal in R to handle reading such data into R.

JSON stands for **JavaScript Object Notation** and is a
language-independent data format, derived from *JavaScript*, but is an
open standard file and data interchange format used to store and
transmit data. Simply put, JSON files are plain text that are readable
by humans and can be easily opened and inspected. JSON data files use
the extension `.json` and encode attribute-value pairs, such as
`"firstName": "Zachary"`, simple array data types such as `"sentence":
["Hello", "World", "I", "am", "Zack"]`, to more complicated data
structures or objects that can be stored (in a file or buffer),
transmitted (via network connection) and reconstructed later, back into
a more complicated structure, like a matrix, data frame, or tibble,
potentially in a completely different environment. JSON was developed in
part to solve the problem for the need of a real-time server-to-browser
communication protocol which was independent of popular browser-plugins
applets like Flash and Java, which were popular at the start of the new
millenium. This is exactly why JSON has become so popular with widespead
use - virtually every programming language can process and deal with
plain text data, and the files are easy to transfer over the internet.

### JSON Data Types

JSON has 5 basic data types: numbers, strings, boolean or logical
(including the empty value, `NULL`), arrays, and objects. These 5 basic
JSON data types, adapted from the [JSON wikipedia
page](https://en.wikipedia.org/wiki/JSON) are outlined below. Whitespace
special characters such as spaces, horizontal tabs, newlines, and
returns are allowed and typically ignored outside of values and
punctuation marks, but are maintained when contained within string
values for instance.

  - Number: a signed decimal number (that can consist of a fractional
    component and/or exponential component), but cannot be a non-number.
  - String: a sequence of Unicode characters, delimited by
    double-quotation marks, and supports escaping syntax utilizing a
    backslash.
  - Boolean: (or Logical) dichotomous data type or flag consisting of
    one of two values: `TRUE` or `FALSE`. Also allows for empty value
    `NULL`.
  - Array: an ordered list of values where each one may be a different
    type, delimited using square bracket notation with elements
    separated by commas.
  - Object: a collection of name–value string pairs, delimited by curly
    brackets and separated by commas. Within each pair, a colon
    character separates the name from its value.

### So where is JSON used?

A great little quick guide tutorial and overview on JSON can be found
[here](https://www.tutorialspoint.com/json/json_quick_guide.htm#:~:text=JSON%20format%20is%20used%20for,used%20with%20modern%20programming%20languages.)
if you are interested in a more extensive dive with some examples and
applications using JSON in several programming languages. There are
several areas where JSON is used, however, we’ve already discussed the
main one which is primarily for used for serializing and transmitting
structured plain-text data over a network connection, transmitting this
data between a server and various web applications, serving as an
alternative to, and in some regards seems to be slowly replacing
Extensible Markup Language (XML). For more on this somewhat heated
debate, check out these articles:

  - [XML is toast, long live
    JSON](https://www.cio.com/article/3082084/xml-is-toast-long-live-json.html)
  - [JSON vs XML](https://hackr.io/blog/json-vs-xml)
  - [Has JSON Overtaken
    XML](https://www.c-sharpcorner.com/article/is-json-overridden-xml/#:~:text=XML%20has%20done%20a%20lot,still%20exist%20in%20this%20world.)
  - [Is XML Dying?](https://blog.submain.com/json-and-xml-dying/)

Tied to the reason above, web services and Application Programming
Interfaces (APIs) use JSON format to provide public data. Thus, it is
used nearly everywhere, in particular in areas which were previously
troubling such as with cross-domain communication. As mentioned
previously, with the dawn of JavaScript and XML, AJAX (Asynchronous
JavaScript and XML), it became of greater importance for websites to be
able to load data sources quickly, asynchronously, or in the background
- all without slowing down the speed of loading and rendering the page.
Modern sites, like for example Twitter, use RSS (**R**esource
Description Framework **S**ite **S**ummary) feeds, which is essentially
XML-formatted plain text originally designed as a metadata data model.
Before we get too far down the proverbial rabbit hole, RSS feeds became
popular because on the server-side they are easy to use and import.
However, if you try to use AJAX on them, you will get errors, because
they are only able to load in the same domain the request is being made
from - generally querys will result in errors. This is truly where JSON
shines, overcoming this inability to communicate across domains, and
therefore is able to send data packets back to the domain making the
call or query. This is why we will be using JSON to query the NHL Hockey
API without worry.

### A good way to store data?

For all of the reasons we have discussed above, JSON is a great way to
store data because it is light-weight, easy to read and write,
plain-text based, highly interoperable since it is program language
independent, and overcomes the cross-domain communication barrier -
where JSON files are easily sent between servers. This may not be true
for all data - especially extremely private data like social security
numbers and income records for example. Although JSON would be able to
handle the data, you would probably want this type of data stored on
more securely encrypted servers, which are not easily accessed. But
public data, especially governmental data, national health organization
data, or even as in our example here, major league sports team data,
JSON is completely appropriate and easy to use, besides being a great
way to store data.

## Possible R Packages for JSON

So now that we have come to a basic level of understanding what JSON
data is, where it is used, and why it is a good way to store data, we
need to learn how to deal with JSON data. This portion of the discussion
is highly programming language specific, and in our case for the rest of
this vignette, the R programming language will be the method of choice.
Thus, to handle JSON data in R, it is necessary to discuss the packages
and libraries, along with their associated functions, which were
designed and made available for reading in JSON data in R. There are
several major packages for handling JSON data in R, which are as
follows:

  - `rjson`
  - `RJSONIO`
  - `jsonlite`
  - `tidyjson`

There is a great resource, [A biased comparsion of JSON packages in
R](), which compares the top three packages listed above, and is in
favor of `jsonlite`, which I would have to agree with, out of that list
jsonlite is by far the best, for some of the reasons mentioned in that
resource.

However, published on the [CRAN R Project
website](https://cran.r-project.org/) a little under two weeks ago, is a
new package `tidyjson` which seems to have some promise. What is
especially intriguing is the writeup on the `tidyjson` package page
itself which can be found
[here](https://cran.r-project.org/web/packages/tidyjson/vignettes/introduction-to-tidyjson.html#:~:text=Tidyjson%20provides%20a%20grammar%20for,work%20with%20in%20the%20tidyverse.)
I am presenting it here so that you know it is available as a resource.
With it being so new, I believe it is best to utilize `jsonlite`.

*NEED REASONS FOR CHOOSING JSONLITE*
