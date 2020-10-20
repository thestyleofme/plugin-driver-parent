package com.github.thestyleofme.driver.core.domain.entity;

import com.github.thestyleofme.driver.core.infra.privilege.Account;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/10/20 16:05
 * @since 1.0.0
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class AccountGroup {

    private Account newA;
    private Account oldA;
}
