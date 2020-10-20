package com.github.thestyleofme.driver.es6.model;

import java.io.Serializable;

import lombok.Data;

/**
 * {
 * "columns": [
 * {
 * "name": "author",
 * "type": "text"
 * },
 * {
 * "name": "name",
 * "type": "text"
 * },
 * {
 * "name": "page_count",
 * "type": "long"
 * },
 * {
 * "name": "release_date",
 * "type": "date"
 * }
 * ],
 * "rows": [
 * [
 * "Dan Simmons",
 * "Hyperion",
 * 482,
 * "1989-05-26T00:00:00.000Z"
 * ],
 * [
 * "James S.A. Corey",
 * "Leviathan Wakes",
 * 561,
 * "2011-06-02T00:00:00.000Z"
 * ],
 * [
 * "Frank Herbert",
 * "Dune",
 * 604,
 * "1965-06-01T00:00:00.000Z"
 * ]
 * ]
 * }
 *
 * @author isaac 2020/8/20 19:52
 * @since 1.0.0
 */
@Data
public class Column implements Serializable {

    private String name;

    private String type;

}
