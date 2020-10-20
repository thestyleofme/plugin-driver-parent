package com.github.thestyleofme.driver.es6.model;

import java.io.Serializable;
import java.util.HashMap;

/**
 * 数据
 *
 * {
 *     "library": {
 *         "mappings": {
 *             "book": {
 *                 "properties": {
 *                     "author": {
 *                         "type": "text",
 *                         "fields": {
 *                             "keyword": {
 *                                 "type": "keyword",
 *                                 "ignore_above": 256
 *                             }
 *                         }
 *                     },
 *                     "name": {
 *                         "type": "text",
 *                         "fields": {
 *                             "keyword": {
 *                                 "type": "keyword",
 *                                 "ignore_above": 256
 *                             }
 *                         }
 *                     },
 *                     "page_count": {
 *                         "type": "long"
 *                     },
 *                     "release_date": {
 *                         "type": "date"
 *                     }
 *                 }
 *             }
 *         }
 *     }
 * }
 *
 * @author 奔波儿灞
 * @since 1.0
 */
public class Result extends HashMap<String, Index> implements Serializable {
}
